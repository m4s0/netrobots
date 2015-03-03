"""Rabbit uses a simple strategy: it only runs randomly."""

import sys
import os
lib_path = os.path.abspath(os.path.join('..', '..'))
sys.path.append(lib_path)

from math import atan2, degrees
from random import randint
import sys
import client.connect as connect

__author__ = 'roberto'

BASE = 'tcp://127.0.0.1:5555'

def distance(x0, y0, x1, y1):
    return ((x1 - x0) ** 2 + (y1 - y0) ** 2) ** 0.5

def goto(robot, x, y):
    data = robot.wait()
    dx = x - data.x
    dy = y - data.y
    heading = degrees(atan2(dy, dx))

    robot.drive(100, heading)
    data = robot.wait()

    # 80 break distance
    while distance(data.x, data.y, x, y) > 72.1 and data.speed > 0:  # breaking distance = approx 72.1 m
        data = robot.wait()

    robot.drive(0, heading)
    data = robot.wait()
    while data.speed > 0:
        data = robot.wait() # wait speed down

    return not data.dead

def main(argv):
    # create robot

    robot = connect.Connect(BASE)
    status = robot.create_robot(robot.default_creation_params("Rabbit"))

    print "\nStatus: " + robot.show_status(status) + "\n"

    # main loop - goto random position
    while goto(robot, randint(100, 900), randint(100, 900)):
            pass

    robot.delete_robot()

if __name__ == '__main__':
    main(sys.argv)