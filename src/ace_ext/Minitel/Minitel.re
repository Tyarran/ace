open Base;

type color =
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | Default;

type kind =
  | Text(list(ANSITerminal.style), string);
type t = list(kind);

let to_ansiterminal_color = color => {
  switch (color) {
  | Black => ANSITerminal.black
  | Red => ANSITerminal.red
  | Green => ANSITerminal.green
  | Yellow => ANSITerminal.yellow
  | Blue => ANSITerminal.blue
  | Magenta => ANSITerminal.magenta
  | Cyan => ANSITerminal.cyan
  | White => ANSITerminal.white
  | Default => ANSITerminal.default
  };
};

let minitel = (~color=Default, ~children=[], ()) => {
  let style = [to_ansiterminal_color(color)];
  let txt =
    children
    |> List.map(~f=child => {ANSITerminal.sprintf(style, "%s", child)})
    |> String.concat(~sep="");
  ANSITerminal.sprintf(style, "%s", txt);
};
