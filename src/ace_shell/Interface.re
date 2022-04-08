open Ace_lib.Types;
open Base;

let wrap_markup = (value, markup, _start, _end) => {
  LTerm.(
    LTerm_text.(
      switch (value) {
      | None => markup
      | Some(value) =>
        List.append(List.append([_start(value)], markup), [_end])
      }
    )
  );
};

let fg = (color, markup) =>
  wrap_markup(
    color,
    markup,
    color => LTerm_text.B_fg(color),
    LTerm_text.E_fg,
  );
let bg = (color, markup) =>
  wrap_markup(
    color,
    markup,
    color => LTerm_text.B_bg(color),
    LTerm_text.E_bg,
  );
let b = (value, markup) =>
  wrap_markup(
    value,
    markup,
    value => LTerm_text.B_bold(value),
    LTerm_text.E_bold,
  );
let u = (value, markup) =>
  wrap_markup(
    value,
    markup,
    value => LTerm_text.B_underline(value),
    LTerm_text.E_underline,
  );
let bk = (value, markup) =>
  wrap_markup(
    value,
    markup,
    value => LTerm_text.B_blink(value),
    LTerm_text.E_blink,
  );
let r = (value, markup) =>
  wrap_markup(
    value,
    markup,
    value => LTerm_text.B_reverse(value),
    LTerm_text.E_reverse,
  );

module Text = {
  let createElement = (~children=[], ()) => {
    List.map(children, ~f=child => LTerm_text.S(child));
  };
};

module Style = {
  let createElement =
      (
        ~foreground=?,
        ~background=?,
        ~bold=false,
        ~underline=false,
        ~blink=false,
        ~reverse=false,
        ~children=[],
        (),
      ) => {
    List.fold(
      children,
      ~f=(acc, child) => List.append(acc, child),
      ~init=[],
    )
    |> fg(foreground)
    |> bg(background)
    |> b(Some(bold))
    |> u(Some(underline))
    |> bk(Some(blink))
    |> r(Some(reverse));
  };
};

let print = msg => Stdio.Out_channel.output_string(Stdio.stdout, msg);
let flush = () => Stdio.Out_channel.flush(Stdio.stdout);

let welcome = (config: Config.t) => {
  Ministel.(
    <terminal>
      <line>
        <text> "Welcome to the " </text>
        <text color=Green> "ace shell " </text>
        <text color=Red> {"(v" ++ config.version ++ ")"} </text>
      </line>
      <br />
    </terminal>
  );
};

let message = (~value, arrow_color, step_color, value_color, step_text) => {
  let text_ = Poly.(value != None) ? step_text ++ ": " : step_text;
  Ministel.(
    <terminal>
      <line>
        <text color=arrow_color> "=> " </text>
        <text color=step_color> text_ </text>
        <text color=value_color> {Option.value(value, ~default="")} </text>
      </line>
    </terminal>
  );
};

let success = (~value=?, step_text) =>
  message(~value, Green, Green, Green, step_text);

let error = (~value=?, step_text) =>
  message(Red, White, Red, step_text, ~value);

let debug = (~value, step_text) =>
  message(Yellow, White, Yellow, step_text, ~value);

let prompt = line_number => {
  let name =
    Ace_lib.Utils.expand_name("you", ["you", "Jean Tibote", "debug"]);
  /* let formated_line_number = Int.to_string(line_number); */
  let markup =
    <Style foreground=LTerm_style.green>
      /* <Style foreground=LTerm_style.yellow> */
      /* <Text> {"[" ++ formated_line_number ++ "]"} </Text> */
      /* </Style> */
       <Text> {name ++ " > "} </Text> </Style>;
  LTerm_text.eval(markup);
};
