open Ace_lib.Types;
open Base;
open Minitel;

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

let line = (~children=[], ()) => {
  children |> String.concat(~sep="") |> (++)("\r\n");
};

let welcome = (config: Config.t) => {
  "Welcome to the "
  ++ <minitel fg=Green> "ace shell " {"(v" ++ config.version ++ ")"} </minitel>;
};

let space = name =>
  Array.create(~len=String.length(name ++ " > "), ' ')
  |> Array.to_list
  |> String.of_char_list;

let rec expand = (name, prefix, count) => {
  switch (String.length(name) <= count) {
  | false => name
  | true => expand(prefix ++ name, prefix, count)
  };
};

let expand_name = (name, name_list) => {
  name_list
  |> List.fold(~init=0, ~f=(acc, name) =>
       Int.max(acc, String.length(name))
     )
  |> expand(name, " ");
};

let message = (~value, arrow_color, step_color, value_color, step_text) => {
  let text_ = Poly.(value != None) ? step_text ++ ": " : step_text;
  <line>
    <minitel fg=arrow_color> "=> " </minitel>
    <minitel fg=step_color> text_ </minitel>
    <minitel fg=value_color> {Option.value(value, ~default="")} </minitel>
  </line>;
};

module Message = {
  type message_type =
    | Error
    | Warning
    | Success;

  let message = (type_, text, value) => {
    let color =
      switch (type_) {
      | Success => Green
      | Error => Red
      | Warning => Yellow
      };

    let formated_value =
      switch (value) {
      | "" => ""
      | _other_string =>
        <minitel>
          <minitel fg=White> ": " </minitel>
          <minitel fg=color> value </minitel>
        </minitel>
      };

    <line>
      <minitel fg=color> "=> " </minitel>
      <minitel fg=White> text </minitel>
      formated_value
    </line>;
  };

  let success = message(Success);
  let error = message(Error);
  let warning = message(Warning);
};

let line_with_space = (~color, ~user, ~children=[], ()) => {
  let space = space(user);
  children
  |> List.map(~f=child => <minitel fg=color> {space ++ child} </minitel>)
  |> String.concat;
};

let chat_line = (~color, ~user, ~children=[], ()) => {
  let (first, other_lines) =
    switch (children) {
    | [] => ("", "")
    | [child] => (<minitel fg=color> child </minitel>, "")
    | [first, ...rest] => (
        <minitel fg=color> first </minitel>,
        <line_with_space color user> ...rest </line_with_space>,
      )
    };
  <minitel>
    <minitel fg=color> {user ++ " > " ++ first} </minitel>
    other_lines
  </minitel>;
};

let debug = (~step, ~value, ~children, ~user, ()) => {
  let message = message(~value, Yellow, Yellow, Yellow, step);
  <chat_line user color=Yellow> message </chat_line>;
};

let debug_info = (context: Context.t) => {
  let _ =
    Lwt.(
      [
        Message.warning("botname", context.config.bot.name),
        Message.warning("input", In.get_text(context.input)),
        Message.warning("origin", In.get_service_name(context.input)),
        Message.warning("action", context.action.name),
        Message.warning("event", Event.to_string(context.event)),
        Message.warning(
          "runner",
          Action.Runner.to_string(context.action.runner),
        ),
        Message.warning("destination", "shell"),
      ]
      |> String.concat(~sep="")
      |> Lwt_io.write(Lwt_io.stdout)
    );
  ();
};

let success = (~value=?, step_text) =>
  message(~value, Green, Green, Green, step_text);

let error = (~value=?, step_text) =>
  message(Red, White, Red, step_text, ~value);

let debug = (~value, step_text) =>
  message(Yellow, White, Yellow, step_text, ~value);

let prompt = line_number => {
  let name = expand_name("you", ["you", "Jean Tibote", "debug"]);
  /* let formated_line_number = Int.to_string(line_number); */
  let markup =
    <Style foreground=LTerm_style.green>
      /* <Style foreground=LTerm_style.yellow> */
      /* <Text> {"[" ++ formated_line_number ++ "]"} </Text> */
      /* </Style> */
       <Text> {name ++ " > "} </Text> </Style>;
  LTerm_text.eval(markup);
};
