open Types;

type widget =
  | Simple_text(Context.t, string)
  | Command_result(Context.t, string);

let render: (widget, Types.Service.t) => Types.widget_result;
