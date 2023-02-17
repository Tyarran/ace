open Alcotest;
open Lwt.Infix;

module To_test = {
  let run = Ace.Runner.run;
};

module RunAction = {
  let test_run_ping = (_, ()) => {
    let action =
      Ace.Message.Action.{
        name: "test action",
        only_from: None,
        runner: Internal(Ping),
        trigger: Command("ping"),
      };

    To_test.run(action)
    >>= (
      action_res => {
        switch (action_res) {
        | Ok(Bool(result)) =>
          check(bool, "should be true", true, result);
          Lwt.return();
        | _ => failwith("sh;
        ould not be an error")
        };
      }
    );
  };

  let test_run_help = (_, ()) => {
    let action =
      Ace.Message.Action.{
        name: "test action",
        only_from: None,
        runner: Internal(Help),
        trigger: Command("Help"),
      };

    To_test.run(action)
    >>= (
      action_res => {
        switch (action_res) {
        | Ok(ListKeyValue(result)) =>
          check(
            string,
            "should be the same",
            "Available commands:",
            result.intro,
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
            "run ping internal command",
            `Quick,
            RunAction.test_run_ping,
          ),
          Alcotest_lwt.test_case(
            "run help internal command",
            `Quick,
            RunAction.test_run_help,
          ),
        ],
      ),
    ],
  );
