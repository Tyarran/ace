open Ace_lib;
open Ace_lib.Types;

open Alcotest;

module To_test = {
  let render = Ace_lib.Widget.render;
};

module RenderShell = {
  let test_render_command_result = () => {
    let command_result =
      Response.{
        result: Ok(Response.CommandResult(Ok("pong"))),
        destination: Message.Action.Shell,
      };

    let result = To_test.render(command_result);

    check(string, "rendered command result", "pong", result);
  };

  let test_render_user_manual = () => {
    let command_result =
      Response.{
        result:
          Ok(
            UserManualResponse(
              Ok({
                intro: None,
                title: Some("Available commands"),
                values: [("ping", "ping")],
              }),
            ),
          ),
        destination: Message.Action.Shell,
      };

    let result = To_test.render(command_result);

    check(
      string,
      "rendered user manual result",
      "User manual response",
      result,
    );
  };

  let test_render_debug = () => {
    let raw = "ping";
    let input = Message.Input.from_shell(raw);
    let message = Message.{input, value: Message.Command("ping", [])};
    let action =
      Message.Action.make(
        "ping",
        None,
        Internal(Help),
        Command("ping"),
        (),
      );

    let command_result =
      Response.{
        result: Ok(Debug(Ok({raw, message, input, action}))),
        destination: Message.Action.Shell,
      };

    let result = To_test.render(command_result);

    check(string, "rendered debug result", "debug", result);
  };
};

run(
  "Widget",
  [
    (
      "render",
      [
        test_case(
          "command result for shell output",
          `Quick,
          RenderShell.test_render_command_result,
        ),
        test_case(
          "debug result for shell output",
          `Quick,
          RenderShell.test_render_debug,
        ),
        test_case(
          "user manual result for shell output",
          `Quick,
          RenderShell.test_render_user_manual,
        ),
      ],
    ),
  ],
);
