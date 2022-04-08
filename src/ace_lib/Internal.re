open Base;
open Types;

let ping_responses = [|"pong 🏓", "pong", "🏓"|];

let unknown_responses = [|
  "Je n'ai pas compris",
  "J'essaye de me concentrer mais franchement, je n'arrive pas à comprendre la demande",
  "hum...",
  "Je doute ... 🤔",
  "T'es sûr de ce que tu veux ? Parce que moi, je n'ai rien compris",
  "Je ne sais vraiment pas quoi faire de ça ...🤷",
  "Soit je suis mal codé, soit tu fais une erreur. Mais en tout cas, je ne sais pas quoi faire de ta demande",
|];

let random_choice = array => {
  let choice = Random.int(Array.length(array));
  array[choice];
};

let execute_ping = context => {
  let random = random_choice(ping_responses);
  let command_response = Widgets.Command_result(context, random);
  Lwt_result.return(command_response);
};

let execute_unknown = context =>
  Lwt_result.return(
    Widgets.Simple_text(context, random_choice(unknown_responses)),
  );

let execute = (context: Context.t, command_name) => {
  switch (command_name) {
  | "ping" => execute_ping(context)
  | _ => execute_unknown(context)
  };
};
