open Base;
open Types;

module CommandResultWidget = {
  let render_shell = (res: Result.t(string, string)) => {
    switch (res) {
    | Ok(value) => value
    | Error(msg) => msg
    };
  };

  let render = (res, destination: Message.Action.provider) => {
    switch (destination) {
    | Shell => render_shell(res)
    };
  };
};

module UserManualWidget = {
  let render_shell = _res => {
    "User manual response";
  };

  let render = (res, destination: Message.Action.provider) => {
    switch (destination) {
    | Shell => render_shell(res)
    };
  };
};

module DebugWidget = {
  let render_shell = _res => {
    "debug";
  };

  let render = (res, destination: Message.Action.provider) => {
    switch (destination) {
    | Shell => render_shell(res)
    };
  };
};

module ErrorWidget = {
  let render_shell = _value => {
    "error";
  };

  let render = (value, destination: Message.Action.provider) => {
    switch (destination) {
    | Shell => render_shell(value)
    };
  };
};

let render = (response: Response.t) => {
  switch (response) {
  | Response.{result: Ok(CommandResult(value)), destination} =>
    CommandResultWidget.render(value, destination)
  | Response.{result: Ok(Debug(value)), destination} =>
    DebugWidget.render(value, destination)
  | Response.{result: Ok(UserManualResponse(value)), destination} =>
    UserManualWidget.render(value, destination)
  | Response.{result: Error(message), destination} =>
    ErrorWidget.render(message, destination)
  };
};
