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
    | UserManualResponse(user_manual_response)
    | Debug(debug)
    | Bool(bool);

  type t = Result.t(response_type, string);
};
