#include <stdlib.h>

#include "game.h"

Game* NewGame()
{
    Game* game = (Game*)malloc(sizeof(Game));
    for (int i = 0; i < 12; i++) {
        game->house[i] = 4;
    }
    game->score1 = 0;
    game->score2 = 0;

    return game;
}

int EndGame(Game* game)
{
    if(game){
        free(game);
        return 1;
    }
    return 0;
}

