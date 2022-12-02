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
  | Black => ANSITerminal.Black
  | Red => ANSITerminal.Red
  | Green => ANSITerminal.Green
  | Yellow => ANSITerminal.Yellow
  | Blue => ANSITerminal.Blue
  | Magenta => ANSITerminal.Magenta
  | Cyan => ANSITerminal.Cyan
  | White => ANSITerminal.White
  | Default => ANSITerminal.Default
  };
};

let add_style = (value, style, styles) => {
  value ? List.append(styles, [style]) : styles;
};

let minitel =
    (
      ~fg=Default,
      ~bg=Default,
      ~b=false,
      ~u=false,
      ~bk=false,
      ~i=false,
      ~h=false,
      ~r=true,
      ~children=[],
      (),
    ) => {
  let styles =
    []
    |> add_style(r, ANSITerminal.Reset)
    |> add_style(h, ANSITerminal.Hidden)
    |> add_style(i, ANSITerminal.Inverse)
    |> add_style(bk, ANSITerminal.Blink)
    |> add_style(u, ANSITerminal.Underlined)
    |> add_style(b, ANSITerminal.Bold)
    |> add_style(true, ANSITerminal.Foreground(to_ansiterminal_color(fg)))
    |> add_style(true, ANSITerminal.Background(to_ansiterminal_color(bg)));
  let txt =
    children
    |> List.map(~f=child => {ANSITerminal.sprintf(styles, "%s", child)})
    |> String.concat(~sep="");
  ANSITerminal.sprintf(styles, "%s", txt);
};
