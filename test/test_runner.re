open Ace.Types;
open Alcotest;
open Base;
open Lwt.Infix;

module To_test = {
  let run = Ace.Runner.run;
};

let config =
  Ace.Message.Config.{
    actions: [
      Ace.Message.Action.make(
        "test action",
        None,
        Internal(Ping),
        Command("ping"),
        (),
      ),
      Ace.Message.Action.make(
        "test action",
        None,
        Internal(Help),
        Command("Help"),
        (),
      ),
    ],
    default_action:
      Ace.Message.Action.make(
        "test action",
        None,
        Internal(Help),
        Command("Help"),
        (),
      ),
  };

module RunAction = {
  let test_run_ping = (_, ()) => {
    let action =
      Ace.Message.Action.make(
        "test action",
        None,
        Internal(Ping),
        Command("ping"),
        (),
      );
    let message =
      Ace.Message.{
        value: Command("ping", []),
        input: Input.from_shell("ping"),
      };

    To_test.run(config, message, action)
    >>= (
      response_res => {
        switch (response_res) {
        | Ace.Types.Response.{
            result: Ok(CommandResult(Ok(result))),
            destination: _destination,
          } =>
          check(string, "should be \"pong\"", "pong", result);
          Lwt.return();
        | _ => failwith("sh;
                   ould not be an error")
        };
      }
    );
  };

  let test_run_help = (_, ()) => {
    let action =
      Ace.Message.Action.make(
        "test action",
        None,
        Internal(Help),
        Command("Help"),
        (),
      );
    let message =
      Ace.Message.{
        value: Command("help", []),
        input: Input.from_shell("help"),
      };

    To_test.run(config, message, action)
    >>= (
      response_res => {
        switch (response_res) {
        | Response.{
            result: Ok(UserManualResponse(Ok(result))),
            destination: _destination,
          } =>
          check(
            string,
            "should be the same",
            "Available commands:",
            Option.value(result.title, ~default=""),
          );
          Lwt.return();
        | _ => failwith("should not be an error")
        };
      }
    );
  };
};

let () =
  Lwt_main.run @@
  Alcotest_lwt.run(
    "Runner",
    [
      (
        "run",
        [
          Alcotest_lwt.test_case(
            "ping internal command",
            `Quick,
            RunAction.test_run_ping,
          ),
          Alcotest_lwt.test_case(
            "help internal command",
            `Quick,
            RunAction.test_run_help,
          ),
        ],
      ),
    ],
  );
