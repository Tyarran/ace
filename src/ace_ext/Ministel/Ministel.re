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

let terminal = (~color=Default, ~children=[], ()) => {
  let style = [color |> to_ansiterminal_color];
  let children_text = String.concat(~sep="", children);
  ANSITerminal.sprintf(style, "%s", children_text);
};

let line = (~children=[], ()) => {
  let children_text = String.concat(~sep="", children);
  children_text ++ "\r\n";
};

let text = (~color=Default, ~children=[], ()) => {
  let style = [color |> to_ansiterminal_color];
  let children_text = String.concat(~sep="", children);
  ANSITerminal.sprintf(style, "%s", children_text);
};

let br = (~children=[], ()) => {
  let _ = children;
  "\r\n";
};
