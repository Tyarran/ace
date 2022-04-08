open Base;
open Types;

type widget =
  | Simple_text(Context.t, string)
  | Command_result(Context.t, string);

module type Renderable = {
  type response;

  let render_shell: (Context.t, response) => string;
};

module Renderer = (Widget: Renderable) => {
  let render =
      (
        context: Context.t,
        response: Widget.response,
        destination: Types.Service.t,
      ) => {
    Types.Service.(
      switch (destination) {
      | Shell => Widget.render_shell(context, response)
      | _ => Widget.render_shell(context, response)
      }
    );
  };
};

module SimpleTextRenderer = Renderer(SimpleTextWidget);
module CommandResultRenderer = Renderer(CommandResultWidget);

let render = (widget, destination) => {
  switch (widget) {
  | Simple_text(context, response) =>
    SimpleTextRenderer.render(context, response, destination)
  | Command_result(context, response) =>
    CommandResultRenderer.render(context, response, destination)
  };
};
