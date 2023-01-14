open Extensions
open Model
open Opal

let digits =
  spaces >> many1 digit => String.implode => int_of_string


let ore_robot =
  spaces >> token "Each ore robot costs" >> spaces >> digits << spaces << token "ore."

let clay_robot =
  spaces >> token "Each clay robot costs" >> spaces >> digits << spaces << token "ore."


let obsidian_robot =
  spaces >> token "Each obsidian robot costs" >> spaces >> digits << spaces << token "ore" >>= fun ore ->
  spaces >> token "and" >> spaces >> digits << spaces << token "clay." >>= fun clay ->
  return (ore, clay)


let geode_robot =
  spaces >> token "Each geode robot costs" >> spaces >> digits << spaces << token "ore" >>= fun ore ->
  spaces >> token "and" >> spaces >> digits << spaces << token "obsidian." >>= fun obsidian ->
  return (ore, obsidian)


let headline =
  spaces >> token "Blueprint" >> spaces >> digits << spaces << token ":"


let blueprint =
  headline >>= fun number ->
  ore_robot >>= fun ore_ore ->
  clay_robot >>= fun clay_ore ->
  obsidian_robot >>= fun (obsidian_ore, obsidian_clay) ->
  geode_robot >>= fun (geode_ore, geode_obsidian) ->
  return {
    number;
    ore_ore;
    clay_ore;
    obsidian_ore;
    obsidian_clay;
    geode_ore;
    geode_obsidian
  }


let blueprint_list =
  many1 blueprint
  

let parse =
  Opal.parse blueprint_list
