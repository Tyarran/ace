open Types;
open Base;

let render_shell = (context: Context.t, response_text: string) => {
  Types.ChatLine(
    context,
    context.config.bot.name,
    ["Action: " ++ context.action.name, "result:", response_text],
  );
};
