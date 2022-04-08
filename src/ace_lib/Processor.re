open Base;
open Lwt;
open Re2.Exceptions;
open Re2;
open Types.Action;
open Types;

type processing_error =
  | Invalid_message(string)
  | Invalid_command(string, string);

type message_kind =
  | Mk_command(string, list(string))
  | Mk_text(string);

/* type processing_result = */
/*   Result.t(Lwt_result.t(string, string), processing_error); */
/*  */
let command_regex = Re2.create_exn("^\\!(?P<command>\\w+)\\ *(?P<args>.*)$");

let filter_empty_string = string_list =>
  List.filter(string_list, ~f=item => String.compare(item, "") != 0);

let parse_command = text => {
  switch (find_submatches_exn(command_regex, text)) {
  | subs =>
    let command = Option.value(subs[1], ~default="");
    let args_sub = subs[2];
    let args =
      switch (args_sub) {
      | Some(sub) => sub |> String.split(~on=' ') |> filter_empty_string
      | None => []
      };
    Ok((command, args));
  | exception (Regex_match_failed(message)) => Error(message)
  };
};

let process_command = text =>
  switch (parse_command(text)) {
  | Ok((name, args)) => Ok(Mk_command(name, args))
  | Error(message) => Error(Invalid_command(text, message))
  };

let process_message = message =>
  switch (message) {
  | string when String.is_prefix(message, ~prefix="!") =>
    process_command(message)
  | _other => Ok(Mk_text(message))
  };

let find_command_action = (actions: array(Action.t), name, ~default) => {
  actions
  |> Array.find_map(~f=action => {
       let event_opt =
         action.on
         |> List.find(~f=event => {
              switch (event) {
              | Event.Command(command_name) when Poly.(command_name == name) =>
                true
              | _other => false
              }
            });
       switch (event_opt) {
       | Some(event) => Some((action, event))
       | None => None
       };
     })
  |> Option.value(~default);
};

let find_action = (actions, kind, ~default) => {
  switch (kind) {
  | Mk_command(name, _) => Ok(find_command_action(actions, name, ~default))
  | _other => Ok(default)
  };
};

let build_thread = (context, action) => {
  switch (action.runner) {
  | Runner.Internal(command) => Internal.execute(context, command)
  | Runner.HttpResponse(url, method, allowed, params, headers) =>
    HTTPResponse.execute_http_request(
      context,
      url,
      method,
      allowed,
      params,
      headers,
    )
  };
};

let get_thread = (config: Config.t, input, ~default) => {
  open Result;

  let action_res =
    input
    |> Out.get_text
    |> return
    >>= process_message
    >>= find_action(config.actions, ~default);

  switch (action_res) {
  | Ok((action, event)) =>
    let context = Context.{config, input, action, event};
    Ok(build_thread(context, action));
  | Error(error) => Error(error)
  };
};
