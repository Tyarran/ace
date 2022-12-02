open Base;

let render_shell = (context: Types.Context.t, response_text) =>
  Types.ChatLine(context, context.config.bot.name, [response_text]);
