#!/usr/bin/env python
#
# MIT License
#
# Copyright (c) 2018 Fabrizio Colonna <colofabrix@tin.it>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#

from sys import exit
from math import ceil
from time import sleep
from datetime import datetime
from random import seed, randint
from argparse import ArgumentParser

# Precomputed list used to discover the (x, y) coordinates of the neighbours of
# a given one
ADJACENCY = [
    (i, j)
    for i in (-1, 0, 1)
    for j in (-1, 0, 1)
    if not (i == j == 0)
]

def print_field(field):
    """
    Prints the field
    """
    # Clear the screen and move cursor up.
    # See: http://ascii-table.com/ansi-escape-sequences-vt-100.php
    print("\x1b[2J\x1b[H")
    for row in field:
        print(" ".join(row))


def init_field(width, height, filling_percent):
    """
    Initializes the field with random values
    """
    seed(datetime.now())
    rnd_range = max(int(ceil(1.0 / filling_percent)) - 1, 0)
    return [[
            DEAD_SYMBOL if randint(0, rnd_range) else ALIVE_SYMBOL
            for i in range(width)
        ]
        for j in range(height)
    ]


def load_field(field_file):
    """
    Loads a field from a file
    """
    field = []
    with open(field_file, 'r') as ffile:
        for row in ffile.readlines():
            field.append(list(row.strip().replace(' ', '')))
    return field


def get_alive_neighbours(field, row, col):
    """
    Finds how many neighbours of the given cell are alive
    """
    rows, cols = len(field), len(field[0])
    neighbours = [
        field[(row + y) % rows][(col + x) % cols]
        for y, x in ADJACENCY
    ]
    return neighbours.count(ALIVE_SYMBOL)


def get_next_cell_state(cell, alive_neighbours):
    """
    Determines the next state of the cell
    """
    # These are the rules of the game as stated on Wikipedia
    if cell == ALIVE_SYMBOL and (alive_neighbours < 2 or alive_neighbours > 3):
        return DEAD_SYMBOL
    if cell == DEAD_SYMBOL and alive_neighbours == 3:
        return ALIVE_SYMBOL
    return cell


def step_field(field):
    """
    Performs one iteration of the game
    """
    return [[
            get_next_cell_state(
                field[row][col],
                get_alive_neighbours(field, row, col)
            )
            for col in range(len(field[row]))
        ]
        for row in range(len(field))
    ]


# Command line arguments
parser = ArgumentParser()
parser.add_argument('--alive-symbol', default='#')
parser.add_argument('--dead-symbol', default='.')
parser.add_argument('--delay', default=0.5, type=float)
parser.add_argument('--width', default=20,  type=float)
parser.add_argument('--height', default=20,  type=float)
parser.add_argument('--filling', default=0.2, type=float)
parser.add_argument('field_file', nargs="?")
args = parser.parse_args()

# Configuration
ALIVE_SYMBOL = args.alive_symbol
DEAD_SYMBOL = args.dead_symbol

# Initialize the field
field = init_field(args.width, args.height, args.filling)
if args.field_file is not None:
    field = load_field(args.field_file)

# Main loop
while True:
    try:
        print_field(field)
        field = step_field(field)
        sleep(args.delay)
    except KeyboardInterrupt:
        print "Bye"
        exit(0)

# vim: ft=python:ts=4:sw=4
