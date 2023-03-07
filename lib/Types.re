open Base;
open Message;

module Response = {
  type user_manual_response = {
    intro: option(string),
    title: option(string),
    values: list((string, string)),
  };

  type debug = {
    raw: string,
    input: Message.Input.t,
    message: Message.t,
    action: Action.t,
  };

  type response_type =
    | UserManualResponse(Result.t(user_manual_response, string))
    | Debug(Result.t(debug, string))
    | CommandResult(Result.t(string, string));

  type t = {
    result: Result.t(response_type, string),
    destination: Message.Action.provider,
  };
};
