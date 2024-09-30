# OCaml GUI Drawing Program

This project is a simple graphical user interface (GUI) library written in OCaml. The program uses different modules to define a graphics context (`gctx`), implement various widgets, and allow basic drawing functionalities.

## Project Structure

The main files in this project are:

- **gctx.ml**: Defines the `gctx` (graphics context) module
- **gctx.mli**: The interface file for `gctx.ml`
- **paint.ml**: The main file for the paint application
- **widget.ml**: Implements various GUI widgets, including buttons, checkboxes, and layout widgets (e.g., vertical and horizontal pairs of widgets)
- **widget.mli**: The interface file for `widget.ml`

## How to Use

1. **Compilation**: Compile the project using `ocamlc` or `ocamlopt`, along with any necessary dependencies. You can use the following command:
   ```bash
   ocamlc gctx.mli gctx.ml widget.mli widget.ml paint.ml -o paint_app
   ```
2. **Running the Application**: Execute the compiled application:
   ```bash
   ./paint_app
   ```
   This will open a graphical window where you can use the tools to draw shapes.

3. **Drawing Tools**: The application supports various modes such as drawing lines, points, and ellipses. It also provides options to select colors, change line thickness, and undo actions.

## Features

- **Drawing Primitives**: Create lines, points, and ellipses using mouse events.
- **Color and Thickness**: Change the color and thickness of the drawing elements.
- **Undo Functionality**: Revert the last drawn shape.
- **Widgets**: The GUI includes buttons, sliders, checkboxes, and other widgets for an interactive experience.

