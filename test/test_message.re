open Ace;
open Alcotest;

module To_test = {
  let process = Ace.Message.process;
  let find_action = Ace.Message.find_action;
};

/* tests */
let test_process_ping_input = () => {
  let input = Message.Input.from_shell("!ping");

  let message_res = To_test.process(input);

  switch (message_res) {
  | Ok({input: _, value: Command(name, arguments)}) =>
    check(string, "should be ping", "ping", name);
    check(list(string), "should be an empty list", [], arguments);
  | Error(_) => failwith("should not be an error")
  };
};

let test_process_debug_input = () => {
  let input = Message.Input.from_shell("!debug \"!ping one two three\"");

  let message_res = To_test.process(input);

  switch (message_res) {
  | Ok({input: _, value: Command(name, arguments)}) =>
    check(string, "should be debug", "debug", name);
    check(
      list(string),
      "should be a list of one item",
      ["!ping one two three"],
      arguments,
    );
  | Error(_) => failwith("should not be an error")
  };
};

let test_process_input_with_arguments = () => {
  let input = Message.Input.from_shell("!ping one two three");

  let message_res = To_test.process(input);

  switch (message_res) {
  | Ok({input: _, value: Command(name, arguments)}) =>
    check(string, "should be ping", "ping", name);
    check(
      list(string),
      "should be have arguments",
      ["one", "two", "three"],
      arguments,
    );
  | Error(_) => failwith("should not be an error")
  };
};

let test_process_input_invalid_command = () => {
  let input = Message.Input.from_shell("ping one two three");

  let message_res = To_test.process(input);

  switch (message_res) {
  | Error(Message.InvalidCommand(message)) =>
    check(string, "should be the message", "ping one two three", message)
  | _ => failwith("should not be an error")
  };
};

let test_process_input_empty_command = () => {
  let input = Message.Input.from_shell("");

  let message_res = To_test.process(input);

  switch (message_res) {
  | Error(Message.InvalidCommand(message)) =>
    check(string, "should be the message", "", message)
  | _ => failwith("should not be an error")
  };
};

let test_process_input_double_exclamation = () => {
  let input = Message.Input.from_shell("!!ping");

  let message_res = To_test.process(input);

  switch (message_res) {
  | Error(Message.InvalidCommandName(message)) =>
    check(string, "should be the message", "!ping", message)
  | _ => failwith("should not be an error")
  };
};

let test_process_input_double_quoted_argument = () => {
  let input =
    Message.Input.from_shell("!ping one two \"quoted argument\" three");

  let message_res = To_test.process(input);

  switch (message_res) {
  | Ok({input: _, value: Command(name, args)}) =>
    check(string, "should be ping", "ping", name);
    check(
      list(string),
      "should contains one argument",
      ["one", "two", "quoted argument", "three"],
      args,
    );
  | _ => failwith("should not be an error")
  };
};

module FindActionTest = {
  let config =
    Message.Config.{
      actions: [
        Message.Action.make(
          "ping",
          None,
          Internal(Ping),
          Command("ping"),
          (),
        ),
        Message.Action.make(
          "help",
          None,
          Internal(Ping),
          Command("help"),
          (),
        ),
      ],
      default_action:
        Message.Action.make("help", None, Internal(Help), Unknown, ()),
    };

  let test_find_action = () => {
    let message =
      Message.{
        input: Message.Input.from_shell("!ping"),
        value: Message.Command("ping", []),
      };

    let actual = To_test.find_action(config, message);

    check(string, "should be the ping action", "ping", actual.name);
  };

  let test_find_action_default = () => {
    let message =
      Message.{
        input: Message.Input.from_shell("!unknown-command"),
        value: Message.Command("unknown-command", []),
      };

    let actual = To_test.find_action(config, message);

    check(string, "should be the ping action", "help", actual.name);
  };

  let test_find_action_only_slack = () => {
    let message =
      Message.{
        input: Message.Input.from_shell("!ping"),
        value: Message.Command("ping", []),
      };
    let config = {
      ...config,
      actions: [
        Message.Action.make(
          "ping",
          Some([Message.Action.Slack]),
          Internal(Help),
          Command("ping"),
          (),
        ),
      ],
    };

    let actual = To_test.find_action(config, message);

    check(string, "should be the ping action", "help", actual.name);
  };
};

let () =
  run(
    "Processor",
    [
      (
        "process",
        [
          test_case("process ping command", `Quick, test_process_ping_input),
          test_case(
            "process debug command",
            `Quick,
            test_process_debug_input,
          ),
          test_case(
            "with arguments",
            `Quick,
            test_process_input_with_arguments,
          ),
          test_case(
            "with invalid command (missing \"!\")",
            `Quick,
            test_process_input_invalid_command,
          ),
          test_case(
            "with empty command",
            `Quick,
            test_process_input_empty_command,
          ),
          test_case(
            "with invalid command name (double \"!\")",
            `Quick,
            test_process_input_double_exclamation,
          ),
          test_case(
            "with double quoted argument",
            `Quick,
            test_process_input_double_quoted_argument,
          ),
        ],
      ),
      (
        "find_action",
        [
          test_case("simple case", `Quick, FindActionTest.test_find_action),
          test_case(
            "default case",
            `Quick,
            FindActionTest.test_find_action_default,
          ),
          test_case(
            "with only_from filter",
            `Quick,
            FindActionTest.test_find_action_only_slack,
          ),
        ],
      ),
    ],
  );
