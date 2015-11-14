#!/usr/bin/env python3

from __future__ import print_function
from sys import version_info

if version_info.major == 2:
    input = raw_input

invert = lambda board: list(zip(*board))
three = lambda row: row[0] == row[1] == row[2] != ' '
empty = lambda board, indices: board[indices[0]][indices[1]] == ' '


def update(board, indices, player):
    new_board = [[item for item in row] for row in board]
    new_board[indices[0]][indices[1]] = player
    return new_board


def finished(board):
    inverted = invert(board)
    possibilities = map(three, [
        board[0],
        board[1],
        board[2],
        inverted[0],
        inverted[1],
        inverted[2],
        [board[0][0], board[1][1], board[2][2]],
        [board[0][2], board[1][1], board[2][0]]
    ])
    return any(possibilities)


def validated(move):
    split = move.split()
    if len(split) == 2:
        try:
            x = int(split[0])
            y = int(split[1])
            if 0 <= x <= 2 and 0 <= y <= 2:
                return (x, y)
            else:
                raise ValueError("input is invalid")
        except ValueError:
            pass
    return False


def display(board):
    return ('\n-----\n'.join('|'.join(row) for row in board))


def main():
    board = [
        [' ', ' ', ' '],
        [' ', ' ', ' '],
        [' ', ' ', ' ']
    ]
    players = {0: 'x', 1: 'o'}
    player = 1
    while not finished(board):
        player = 0 if player else 1
        print(display(board))
        validated_move = validated(input(
            "Place an %s on the board at: " % players[player]))
        while not (validated_move and empty(board, validated_move)):
            validated_move = validated(input(
                "Your previous move was invalid, please enter another: "))
        board = update(board, validated_move, players[player])
        if finished(board):
            break
    print(display(board))
    print("Player %s wins!" % (player+1))

if __name__ == '__main__':
    main()
