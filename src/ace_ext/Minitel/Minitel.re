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

type style =
  | Bold
  | Underlined
  | Blink
  | Inverse
  | Reset
  | Hidden
  | Foreground(color)
  | Background(color);

type t =
  | Text(string)
  | Style(list(t), list(style));

let add_style = (value, style, styles) =>
  value ? List.append(styles, [style]) : styles;

let make_style = (fg, bg, b, u, bk, i, h, r) => {
  []
  |> add_style(r, Reset)
  |> add_style(h, Hidden)
  |> add_style(i, Inverse)
  |> add_style(bk, Blink)
  |> add_style(u, Underlined)
  |> add_style(b, Bold)
  |> add_style(true, Foreground(fg))
  |> add_style(true, Background(bg));
};

let string = txt => Text(txt);

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
  let styles = make_style(fg, bg, b, u, bk, i, h, r);
  Style(children, styles);
};

/* let minitel = */
/*     ( */
/*       ~fg=Default, */
/*       ~bg=Default, */
/*       ~b=false, */
/*       ~u=false, */
/*       ~bk=false, */
/*       ~i=false, */
/*       ~h=false, */
/*       ~r=true, */
/*       ~children=[], */
/*       (), */
/*     ) => { */
/*   let styles = */
/*     [] */
/*     |> add_style(r, ANSITerminal.Reset) */
/*     |> add_style(h, ANSITerminal.Hidden) */
/*     |> add_style(i, ANSITerminal.Inverse) */
/*     |> add_style(bk, ANSITerminal.Blink) */
/*     |> add_style(u, ANSITerminal.Underlined) */
/*     |> add_style(b, ANSITerminal.Bold) */
/*     |> add_style(true, ANSITerminal.Foreground(to_ansiterminal_color(fg))) */
/*     |> add_style(true, ANSITerminal.Background(to_ansiterminal_color(bg))); */
/*   let txt = */
/*     children */
/*     |> List.map(~f=child => {ANSITerminal.sprintf(styles, "%s", child)}) */
/*     |> String.concat(~sep=""); */
/*   ANSITerminal.sprintf(styles, "%s", txt); */
/* }; */

/* module StringBackend = { */
/*   type t = string; */
/*   let of_color = color => { */
/*     switch (color) { */
/*     | Black => ANSITerminal.Black */
/*     | Red => ANSITerminal.Red */
/*     | Green => ANSITerminal.Green */
/*     | Yellow => ANSITerminal.Yellow */
/*     | Blue => ANSITerminal.Blue */
/*     | Magenta => ANSITerminal.Magenta */
/*     | Cyan => ANSITerminal.Cyan */
/*     | White => ANSITerminal.White */
/*     | Default => ANSITerminal.Default */
/*     }; */
/*   }; */
/*  */
/*   let of_style = style => */
/*     switch (style) { */
/*     | Bold => ANSITerminal.Bold */
/*     | Underlined => ANSITerminal.Underlined */
/*     | Blink => ANSITerminal.Blink */
/*     | Inverse => ANSITerminal.Inverse */
/*     | Reset => ANSITerminal.Reset */
/*     | Hidden => ANSITerminal.Hidden */
/*     | Foreground(color) => ANSITerminal.Foreground(of_color(color)) */
/*     | Background(color) => ANSITerminal.Background(of_color(color)) */
/*     }; */
/*  */
/*   /* let rec render_children = children: list(t) => { */ */
/*   /*   switch (children) { */ */
/*   /*   | [Text(txt)] => txt */ */
/*   /*   | Style => render_children(children) */ */
/*   /*   }; */ */
/*   /* }; */ */
/*   let rec render_children = children => { */
/*     children */
/*     |> List.map(~f=child => { */
/*          switch (child) { */
/*          | Text(txt) => txt */
/*          | Style(children, styles) => */
/*            ANSITerminal.sprintf( List.map(~f=of_style, styles), "%s", render_children(children), */
/*            ) */
/*          } */
/*        }) */
/*   } */
/*  */
/*   let render = styled => { */
/*     switch (styled) { */
/*     | Text(txt) => txt */
/*     | Style(children, styles) => render_children(children) |> String.concat(~sep="") */
/*     }; */
/*   }; */
/*   }; */
/* }; */
