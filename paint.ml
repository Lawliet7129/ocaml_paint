(** The main paint application *)

;; open Gctx
;; open Widget

(******************************************)
(**    SHAPES, MODES, and PROGRAM STATE   *)
(******************************************)

(** A location in the paint_canvas widget *)
type point = position  (* from Gctx *)


(** The shapes that are visible in the paint canvas -- these make up the
    picture that the user has drawn, as well as any other "visible" elements
    that must show up in the canvas area (e.g. a "selection rectangle"). *)

(** The available shapes are modified to include line, points and Ellipses*)
type shape = 
  | Line of {color: color; p1: point;
   p2: point; thickness: thickness}
  | Points of { color: Gctx.color; 
  points: point list; thickness: thickness }
  | Ellipse of {color: Gctx.color; 
  center: position; rx: int;ry: 
  int; thickness: thickness}

(** These are the possible interaction modes that the paint program might be
    in. 

      - LineStartMode means the paint program is waiting for the user to make
        the first click to start a line.

      - LineEndMode means that the paint program is waiting for the user's
        second click. The point associated with this mode stores the location
        of the user's first mouse click.  

      - PointMode means that the paint program is waiting for the user's
        first click. 

      - EllipseStartMode means the paint program is waiting for the user to make
        the first click to start an ellipse.

      - EllipseEndMode means that the paint program is waiting for the user's
        mouse up. The point associated with this mode stores the location
        of the user's first mouse click.       
        *)

type mode = 
  | LineStartMode
  | LineEndMode of point
  | PointMode
  | EllipseStartMode
  | EllipseEndMode of point 

(** The state of the paint program. *)
type state = {
  (** The sequence of all shapes drawn by the user, in order from
      least recent (the head) to most recent (the tail). *)
  shapes : shape Deque.deque;
  mutable preview : shape option;

  (** The input mode the Paint program is in. *)
  mutable mode : mode;

  (** The currently selected pen color. *)
  mutable color : color;

  (* TODO: You will need to add new state for Tasks 2, 5, and *)
  (* possibly 6 *) 

  (** The currently selected pen thickness. *)
  mutable thickness: thickness;
}

(** Initial values of the program state. *)
let paint : state = {
  shapes = Deque.create ();
  mode = LineStartMode;
  color = black;
  preview = None;
  thickness = false;

}



(** This function creates a graphics context with the appropriate
    pen color and thickness. *)

let with_params (g: gctx) (c: color)(t: thickness): gctx =
  let g = with_color g c in
  let g = with_thickness g t in
  g

(*********************************)
(**    MAIN CANVAS REPAINTING    *)
(*********************************)

(** The paint_canvas repaint function.

    This function iterates through all the drawn shapes (in order of least
    recent to most recent so that they are layered on top of one another
    correctly) and uses the Gctx.draw_xyz functions to display them on the
    canvas.  *)

let repaint (g: gctx) : unit =
  let draw_shape (s: shape) : unit =
    begin match s with
      | Line l -> draw_line
         (with_params g l.color l.thickness) l.p1 l.p2
      | Points p -> draw_points 
        (with_params g p.color p.thickness) p.points
      | Ellipse e -> draw_ellipse 
        (with_params g e.color e.thickness) 
        (e.center) e.rx e.ry             
    end in

  let draw_preview (pr: shape option) : unit =
    begin match pr with
    |None -> ()
    |Some (Line k) -> 
      draw_line (with_params g k.color k.thickness) k.p1 k.p2;
    |Some (Points p) -> ()
    |Some (Ellipse e) -> 
    draw_ellipse 
        (with_params g e.color e.thickness) 
        (e.center) e.rx e.ry  
    end in
     Deque.iterate draw_shape paint.shapes;
     draw_preview paint.preview

(** Create the actual paint_canvas widget and its associated
    notifier_controller . *)
let ((paint_canvas : widget), 
(paint_canvas_controller : notifier_controller)) =
  canvas (600, 350) repaint


(************************************)
(**  PAINT CANVAS EVENT HANDLER     *)
(************************************)

(** The paint_action function processes all events that occur
    in the canvas region. *)
let paint_action (gc:gctx) (event:event) : unit =
  let p  = event_pos event gc in   (* mouse position *)
  begin match (event_type event) with
    | MouseDown ->
       (* This case occurs when the mouse has been clicked in the
          canvas, but before the button has been released. How we
          process the event depends on the current mode of the paint
          canvas.  *)
      (begin match paint.mode with 
          | LineStartMode ->
            (* The paint_canvas was waiting for the first click of a line,
              so change it to LineEndMode, recording the starting point of
              the line. *)
            paint.mode <- LineEndMode p
          | LineEndMode p1 ->
            (* The paint_canvas was waiting for the second click of a line,
              so create the line and add it to the deque of shapes. Go back
              to waiting for the first click. *)
            Deque.insert_tail
              (Line {color=paint.color; p1=p1; p2=p; 
              thickness = paint.thickness}) paint.shapes;
            paint.mode <- LineStartMode;
            paint.preview <- Some (Line {color=paint.color;
             p1=p1; p2=p; thickness = paint.thickness});
          | PointMode -> 
            (* The paint_canvas was waiting for the first click of a point,
              so change it to PointMode. *)
                      paint.preview <- 
                      Some (Points {color=paint.color;
                      points=[p]; thickness = paint.thickness})
          |EllipseStartMode -> 
            (* The paint_canvas was waiting for the first click of a point,
              so change it to EllipseStartMode, recording the starting point of
              the ellipse *)
                      paint.mode <- EllipseEndMode p
            (* The paint_canvas was waiting for the Mouse Up of an elipse,
              so create the elipse and add it to the deque of shapes. Go back
              to waiting for the first click. *)
          |EllipseEndMode e1 -> 
                  Deque.insert_tail
                    (Ellipse {color=paint.color; 
                    center =(abs(fst p - fst e1)/2,
                    abs(snd p - snd e1)/2)
                    ;rx=(fst p)/2; ry=(snd p)/2;thickness
                    = paint.thickness}) paint.shapes;
                  paint.mode <- EllipseStartMode;
                  paint.preview <- Some (Ellipse {color=paint.color; 
                  center =(abs(fst p - fst e1)/2,
                  abs(snd p - snd e1)/2)
                    ;rx=abs(fst p - fst e1)/2;
                     ry=abs(snd p - snd e1)/2;
                    thickness = paint.thickness});
       end)

    | MouseDrag -> 
      (* In this case, the mouse has been clicked, and it's being dragged
         with the button down. *)
         ( begin match paint.mode with 
          | LineStartMode -> ()
          | LineEndMode p4 -> paint.preview 
                          <-Some (Line {color=paint.color; p1=p4;
                           p2=p;thickness = paint.thickness});   
          | PointMode -> let points_list =
                          begin match paint.preview with
                         | Some (Points ps) -> ps.points
                         | _ -> []
                        end in
                        paint.preview 
                        <- Some (Points {color=paint.color; 
                        points=p::points_list;
                        thickness = paint.thickness})
          |EllipseStartMode -> ()
          |EllipseEndMode e4 -> 
            paint.preview <- Some (Ellipse {color=paint.color; 
            center =
            (fst e4 + (fst p - fst e4)/2, 
            snd e4 + (snd p - snd e4)/2)
            ;rx=abs(fst p - fst e4)/2; 
            ry=abs(snd p - snd e4)/2;
            thickness = paint.thickness}); 
       end)
     
    | MouseUp -> 
      (* In this case there was a mouse button release event.*)
      (begin match paint.mode with 
          | LineStartMode -> ()
          | LineEndMode p1 -> Deque.insert_tail
              (Line {color=paint.color; p1=p1; 
              p2=p;thickness = paint.thickness}) paint.shapes;
        paint.mode <- LineStartMode;
        paint.preview <- None;
          |PointMode -> 
              let points_list =
                begin match paint.preview with
                 | Some (Points ps) -> ps.points
                 | _ -> []
                  end in
                  paint.preview <- None;
                Deque.insert_tail
              (Points {color=paint.color; points = points_list;
              thickness = paint.thickness}) paint.shapes;
          |EllipseStartMode -> ()
          |EllipseEndMode e4 -> 
             Deque.insert_tail
              (Ellipse {color=paint.color; 
              center =(fst e4 + (fst p - fst e4)/2, snd e4 + (snd p - snd e4)/2)
            ;rx=abs(fst p - fst e4)/2; ry=abs(snd p - snd e4)/2;
            thickness = paint.thickness}) paint.shapes;  
            paint.mode <- EllipseStartMode;
            paint.preview <- None;         
       end)
      
    | _ -> ()
    (* This catches the fMove event (where the user moved the mouse over
       the canvas without pushing any buttons) and the KeyPress event (where
       the user typed a key when the mouse was over the canvas). *)
  end

(** Add the paint_action function as a listener to the paint_canvas *)
;; paint_canvas_controller.add_event_listener paint_action


(**************************************)
(** TOOLBARS AND PAINT PROGRAM LAYOUT *)
(**************************************)

(** This part of the program creates the other widgets for the paint
    program -- the buttons, color selectors, etc., and lays them out
    in the top - level window. *)
(* TODO: Tasks 1, 4, 5, and 6 involve adding new buttons or changing
   the layout of the Paint GUI. Initially the layout is ugly because
   we use only the hpair widget demonstrated in Lecture. Task 1 is to
   make improvements to make the layout more appealing. You may choose
   to arrange the buttons and other GUI elements of the paint program
   however you like (so long as it is easily apparent how to use the
   interface).  The sample screenshot of our solution shows one
   possible design.  Also, feel free to improve the visual components
   of the GUI; for example, our solution puts borders around the
   buttons and uses a custom "color button" that changes its
   appearance based on whether or not the color is currently
   selected. *)

(** Create the Undo button *)
let (w_undo, lc_undo, nc_undo) = button "Undo"

(** This function runs when the Undo button is clicked.
    It simply removes the last shape from the shapes deque. *)
(* TODO: You need to modify this in Task 3 and 4, and potentially 2
   (depending on your implementation). *)

let undo () : unit =
  if Deque.is_empty paint.shapes then () else
    ignore (Deque.remove_tail paint.shapes)

;; nc_undo.add_event_listener (mouseclick_listener undo)

(** Create the Line button *)
let (line_button, _, line_nc)  = button "Line"
let line () : unit =
 paint.mode <- LineStartMode
 ;; line_nc.add_event_listener (mouseclick_listener line)

 
(** Create the Point button *) 
 let (points_button, _, points_nc)  = button "Point"
let point () : unit =
 paint.mode <- PointMode
 ;; points_nc.add_event_listener (mouseclick_listener point)


(** Create the Ellipse button *)
  let (ellipse_button, _, ellipse_nc)  = button "Ellipse"
let ellipse () : unit =
 paint.mode <- EllipseStartMode
 ;; ellipse_nc.add_event_listener (mouseclick_listener ellipse)

(** A spacer widget *)
let spacer : widget = space (10,10)


(** Create the Thick Lines Checkbox *) 
let (switch_w, switch_cb) =
  checkbox false "Thick Lines" 
  ;; switch_cb.add_change_listener (fun x -> paint.thickness <- x)

 (** Create the Red Color Changing Slider *)
let (boxred, vc) = 
  slider 0 "Red" 
  ;;vc.add_change_listener 
  (fun x -> paint.color <- {paint.color with r=255*x/100}) 


 (** Create the Green Color Changing Slider *)
let (boxgreen, vc) = 
  slider 0 "Green" 
  ;;vc.add_change_listener 
  (fun x -> paint.color <- {paint.color with g=255*x/100}) 


 (** Create the Blue Color Changing Slider *)
let (boxblue, vc) = 
  slider 0 "Blue" 
  ;;vc.add_change_listener 
  (fun x -> paint.color <- {paint.color with b=255*x/100}) 


 (** Pairs all the color sliders vertically *)
let color_sliders : widget = 
vlist [boxred; boxgreen; boxblue]




(** The mode toolbar, including points line, ellipse buttons, thick checkbox, 
undo button, and all three color sliders.
 *)
let mode_toolbar : widget = hlist [border (points_button);
                            spacer;border(line_button);
                            spacer; border(ellipse_button); 
                            spacer; border(switch_w);
                             spacer;border(w_undo);spacer;color_sliders]

(* The color selection toolbar. *)
(* This toolbar contains an indicator for the currently selected color
   and some buttons for changing it. Both the indicator and the buttons
   are small square widgets built from this higher-order function. *)
(** Create a widget that displays itself as colored square with the given
    width and color specified by the [get_color] function. *)
let colored_square (width:int) (get_color:unit -> color)
  : widget * notifier_controller =
let repaint_square (gc:gctx) =
  let c = get_color () in
  fill_rect (with_color gc c) (0, 0) (width-1, width-1) in
canvas (width,width) repaint_square

(** The color_indicator repaints itself with the currently selected
    color of the paint application. *)
let color_indicator =
  let indicator,_ = colored_square 24 (fun () -> paint.color) in
  let lab, _ = label "Current Color" in
  border (hpair lab indicator)

(** color_buttons repaint themselves with whatever color they were created
    with. They are also installed with a mouseclick listener
    that changes the selected color of the paint app to their color. *)
let color_button (c: color) : widget =
  let w,nc = colored_square 10 (fun () -> c) in
  nc.add_event_listener (mouseclick_listener (fun () ->
      paint.color <- c ));
  w




    


(** The color selection toolbar. Contains the color indicator and
    buttons for several different colors. *)
(* TODO: Task 1 - This code contains a great deal of boilerplate.  You
     should come up with a better, more elegant, more concise solution...
     Change the color toolbar 
     and mode toolbar in paint.ml to use one Widget.hlist widget each.
Lay out the toolbars and the canvas using a Widget.vlist so that 
the canvas is above the undo button, which is above the color buttons. 
 hpair (hpair color_indicator spacer)
   )
      *)
   let color_toolbar : widget =
   hlist [
   border(color_button black);
   spacer;
   border(color_button white);
   spacer;
   border(color_button red);
   spacer;
   border(color_button green);
   spacer;
   border(color_button blue);
   spacer;
   border(color_button yellow);
   spacer;
   border(color_button cyan);
   spacer;
   border(color_button magenta);
   spacer;
   border(color_indicator)]
  

(** The top-level paint program widget: a combination of the
    mode_toolbar, the color_toolbar and the paint_canvas widgets. *)
(* TODO: Task 1 (and others) involve modifing this layout to add new
   buttons and make the layout more aesthetically appealing. 
   hpair mode_toolbar (hpair spacer (hpair color_toolbar paint_canvas))
   *)
let paint_widget =
  vlist [paint_canvas; spacer;
  mode_toolbar; spacer;  color_toolbar ]
   


(**************************************)
(**      Start the application        *)
(**************************************)

(** Run the event loop to process user events. *)
;; Eventloop.run paint_widget
