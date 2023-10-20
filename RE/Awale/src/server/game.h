#ifndef GAME_H
#define GAME_H

typedef struct
{
    int house[12];
    int score1;
    int score2;
} Game;

Game* NewGame();
int EndGame(Game*);

#endif /* guard */
