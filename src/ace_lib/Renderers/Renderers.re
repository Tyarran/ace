open Types;

module type RendererImpl = {
  let render_text: (Config.t, Interaction.t) => string;
  let render_error: (Config.t, Interaction.t) => string;
  let render_ok: (Config.t, Interaction.t) => string;
};

module Renderer = (Implementation: RendererImpl) => {
  let render = (config: Config.t, interaction: Interaction.t) => {
    let (_, outgoing) = interaction;
    let response = outgoing.response;
    switch (response) {
    | Response.Text(text_res) =>
      switch (text_res) {
      | Ok(text) => Implementation.render_text(config, interaction)
      | Error(text) => Implementation.render_error(config, interaction)
      }
    | Response.Error(_) => Implementation.render_error(config, interaction)
    | Response.Ok_ => Implementation.render_ok(config, interaction)
    };
  };
};

module SlackRenderer = Renderer(SlackRenderer);
