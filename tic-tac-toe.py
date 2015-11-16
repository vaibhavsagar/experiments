#!/usr/bin/env python3

from __future__ import print_function
from sys import version_info

if version_info.major == 2:
    input = raw_input


class Board(object):
    def __init__(self, board=None):
        if board is not None:
            self.board = board
        else:
            self.board = [
                [' ', ' ', ' '],
                [' ', ' ', ' '],
                [' ', ' ', ' ']
            ]

    def __repr__(self):
        return ('\n-----\n'.join('|'.join(row) for row in self.board))

    @staticmethod
    def invert(board):
        return list(zip(*board))

    @staticmethod
    def three(row):
        return row[0] == row[1] == row[2] != ' '

    def empty(self, indices):
        return self.board[indices[0]][indices[1]] == ' '

    def update(self, indices, player):
        self.board[indices[0]][indices[1]] = player

    def finished(self):
        board = self.board
        inverted = self.invert(board)
        possibilities = map(self.three, [
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


def main():
    board = Board()
    players = {0: 'x', 1: 'o'}
    player = 1
    moves = 0
    while not board.finished() and moves < 9:
        player = 0 if player else 1
        print(board)
        validated_move = validated(input(
            "Place an %s on the board at: " % players[player]))
        while not (validated_move and board.empty(validated_move)):
            validated_move = validated(input(
                "Your previous move was invalid, please enter another: "))
        board.update(validated_move, players[player])
        moves += 1
    if board.finished():
        print(board)
        print("Player %s wins!" % (player+1))
    else:
        print("Stalemate!")

if __name__ == '__main__':
    main()
