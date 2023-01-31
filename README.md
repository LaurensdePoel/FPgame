<!-- markdownlint-configure-file {
  "MD013": {
    "code_blocks": false,
    "tables": false
  },
  "MD033": false,
  "MD041": false
} -->

<div align="center">

# Shoot'em Up

The game is based on a **Shoot’em Up** type game, where a player controls a spaceship that needs to clear a level without dying and killing as many enemies as needed. This type of game is typically a side scrolling video game. The twist in this game is that the player is not limited to vertical movement.

One of the main features of the game is that there are multiple types of enemies, with some having their own behavior and/or shooting projectiles. Each level is  defined in a JSON file, which makes adding, removing, and changing waves and levels possible.

![download](https://user-images.githubusercontent.com/40497575/215898919-ad4e6a21-894c-41da-b477-990cc81bc4da.gif)

</div>

## Player
The player is an airplane which is controlled by the player using the key inputs defined in the Chapter Interface. Since the airplane will shoot projectiles automatically, the player must maneuver the plane to avoid getting hit or/and to destroy enemies.

Player 1 &nbsp; ![Player 1](./assets/ships/player_1.bmp)&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
Player 2 &nbsp;![Player 2](./assets/ships/player_2.bmp)

## The enemies
The enemies are not that different from the player, they are also airplanes that shoot automatically and have stats based on their type. Unlike the player, the enemies are controlled by the computer and some enemy types will have a different AI that effects their behavior.

| Enemy                 | Description                                  | Sprite                     |
| --------------------- | -------------------------------------------- | -------------------------- |
| Fighter                 | <ul><li>Basic enemy</li><li>Moderate stats</li><li>moves to a random location on the right side of the screen</li></ul> | ![Fighter](./assets/ships/fighter.bmp) |
| FlyBy               | <ul><li>Moves in a straight line</li><li>High burst fire rate</li><li>Low movement speed</li><li>Low health</li></ul>| ![FlyBy](./assets/ships/flyby.bmp) | 
| Kamikaze               | <ul><li>Tries to collide with the player</li><li>Does not shoot</li><li>High movement speed</li><li>Moderate health</li><li>The damage dealt is equivalent to its remaining hitpoints</li></ul>| ![Kamikaze](./assets/ships/kamikaze.bmp) | 
 
## Power Up
A power up spawns randomly on the playing-field/screen, despawns after 5 seconds or is picked up by the player. When picked up by the player the power up is removed and applied.

| Power Up                 | Description                                  | Sprite                     |
| --------------------- | -------------------------------------------- | -------------------------- |
| Health pack               | Heals the player for 30 hit points. | ![Health pack](./assets/items/health-pack_1.bmp) | 
| Power pack               | Increases the fire rate of the plane, for a few seconds | ![Power pack](./assets/items/power-pack_1.bmp) | 

## Level
This game will have multiple levels. To complete a level, the player has to defeat or evade all enemies. The current level will end if the player’s hit points reach zero before beating the last opponent.
Each level is made out of waves, these waves are predefined in a JSON file. The level loader reads the JSON files and converts them to level records. If there is an error while reading a JSON file an empty level with the number -1 is constructed. The menu in the game shows that the level has an error and the player can’t play the level.

The next wave will spawn when one of these conditions are met:	
- All current enemies are destroyed
- The timer for the wave is over.

### Loading custom levels
The game supports easy custom level loading, since the game automatically converts all level.json files in the folder called “levels/”, the user has to edit or add a new file in this folder and press on reload levels option in the game.

## Commands
```sh
cabal build                         # build project
cabal run                           # startup the game

cabal haddock --haddock-executables # Generate the documentation

cabal clean                         # removes all the generated files
```
