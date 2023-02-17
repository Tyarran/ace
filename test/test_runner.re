open Alcotest;
open Lwt.Infix;

module To_test = {
  open Ace;
  let run = Runner.run;
  let _do_ping = Runner.do_ping;
  let _do_nothing = Runner.do_nothing;
};

module RunAction = {
  let test_run = (_, ()) => {
    let action =
      Ace.Message.Action.{
        name: "test action",
        only_from: None,
        runner: Internal(Ping),
        trigger: Command("ping"),
      };

    action
    |> To_test.run
    >>= (
      action_res => {
        switch (action_res) {
        | Ok(Bool(result)) =>
          Lwt.return(check(bool, "should be true", true, result))
        | _ => failwith("should not be an error")
        };
      }
    );
  };
};

let () =
  Lwt_main.run @@
  Alcotest_lwt.run(
    "Processor",
    [
      (
        "run",
        [Alcotest_lwt.test_case("Initial case", `Quick, RunAction.test_run)],
      ),
    ],
  );
