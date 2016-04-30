'''
    Copyright 2015, 2016 Roberto Gambuzzi <gbinside@gmail.com>
    Copyright 2015, 2016 Massimo Zaniboni <massimo.zaniboni@gmail.com>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
'''

import hashlib
from math import cos, sin, radians, atan2, degrees, pi
from random import randint
import time
from server.rest_api.models.robot_status import RobotStatus
from server.rest_api.models.robot_configuration import RobotConfiguration
from server.rest_api.models.scan_status import ScanStatus
from server.rest_api.models.robot_command import RobotCommand
from server.rest_api.models.drive_command import DriveCommand
from server.rest_api.models.scan_command import ScanCommand

from server.rest_api.models.robot_command import RobotCommand
import uuid

# TODO quando testo per 0 in realta` sono FLOAT quindi arrotondare ad un epsilon o cose del genere

class Board:
    """The Game Model.
    """

    _simulation_tick = 0.125

    _turn_delta_time = 1.0

    def get_turn_delta_time(self):
        """How many seconds there are between robot commands.
        """
        return self._turn_delta_time

    def get_simulation_tick(self):
        """An inner param, about the tick used for simulating the board. Less is this value, more accurate is the simulation, but slower it is the calcs.
        """
        return self._simulation_tick

    def __init__(self, size=(1000, 1000), turn_delta_time = 1.0, simulation_tick = 0.125):
        self._global_time = 0.0
        self._next_turn_time = 0.0
        self._size = size
        self._robots_by_token = {}
        self._missiles = {}
        self._explosions = {}
        self._wall_hit_damage = 2
        self._join_status = None
        self._simulation_tick = simulation_tick
        self._turn_delta_time = turn_delta_time

    def global_time(self):
        return self._global_time

    def simulate_for_a_turn(self):
        """Simulate the board for a turn.
        """

        # Apply all fire commands immediately, because it is the moment in which the robot
        # has more information

        for robot in self._robots_by_token.values():
            """:type robot: Robot"""


        # Apply all the movement commands, then simulate


        tick = self.get_simulation_tick()
        remaining_time = self.get_turn_delta_time()
        while remaining_time > 0:
            if remaining_time >= tick:
                remaining_time = remaining_time - tick
            else:
                tick = remaining_time
                remaining_time = 0
            self.tick(tick)

        # Apply all scan commands, because it is the moment when the scan
        # is more precise.

        # TODO reset the commands for every robot

    def create_robot(self, original_name, configurations):
        """
        Create a new robot with some configurations, and add to the current board.
        Return an inactive status in case of problems.

        :type original_name: str
        :type configurations: RobotConfiguration
        :type timeTick: float
        :type realTimeTick: float
        :rtype: Robot
        """

        # Use a unique name inside the system
        name = original_name
        c = 1
        while name in self.robots:
            c = c + 1
            name = original_name + "_" + str(c)

        robot = Robot(name, uuid.uuid4(), self, configuration=configurations)
        self.add_robot(robot)
        return robot

    def add_robot(self, robot):
        """ Add the robot inside the board.

        :type robot: Robot
        """

        robot.set_board(self)
        self._robots_by_token[robot.get_token()] = robot
        robot._last_command_executed_at_global_time = self.global_time()

    def remove_robot(self, robot):
        """
        :type robot: Robot
        :rtype: bool
        """
        return self.remove_robot_by_token(robot.get_token())

        if robot.get_token() in self._robots_by_token:
            del self._robots_by_token[robot.get_token()]
            return True
        return False

    def remove_robot_by_token(self, token):
        """
        :type token: str
        :rtype: bool
        """
        if token in self._robots_by_token:
            del self._robots_by_token[token]
            return True
        else:
            return False

    def get_robot_by_token(self, token):
        """
        :rtype token: int
        :rtype: Robot
        """
        return self._robots_by_token[token]

    def reinit(self, size=(1000, 1000)):
        """Reinit. Used in debug. """
        self.__init__(size)

    # TODO vedere cosa deve tornare
    def get_status(self):
        ret = dict(
            size=self._size,
            robots=[v.get_status() for v in self.robots.values()],
            missiles=dict([(k, v.get_status()) for k, v in self._missiles.items()]),
            explosions=dict([(k, v.get_status()) for k, v in self._explosions.items()]),
            radar=dict(self._radar),
            kdr=self.kdr,
            global_time=self._global_time,
        )
        return ret

    # TODO essere sicuri di tornare il valore del radar

    def radar(self, scanning_robot, xy, max_scan_distance, degree, resolution):
        """
        :type scanning_robot: Robot
        :type xy: (float, float)
        :type max_scan_distance: float
        :type degree: float
        :type resolution: float
        :rtype: float

        :return: 0 if there is no found object, the object with minimum distance otherwise.
        """

        ret = []
        degree %= 360
        for robot in [x for x in self._robots_by_token.values() if x != scanning_robot]:
            """:type robot: Robot"""

            if robot.is_dead():
                continue
            distance, angle = robot.distance_from_point(xy)
            angle = float(int(180 + angle) % 360)

            if self.angle_distance(angle, degree) > resolution:
                continue
            if distance > max_scan_distance:
                continue
            ret.append(distance)
        return min(ret) if ret else 0.0

    def detect_collision(self, robot, dxy):
        """
        :type robot: Robot
        :type dxy: (float, float)
        :rtype: (float, float, float)
        :return: x, y, damage to apply, or None for no collision
        """

        x, y = robot.get_xy()
        dx, dy = dxy

        # collision with other robots
        step = max(abs(dx), abs(dy))
        if step:  # the robot is moving
            x0, y0 = x - dx, y - dy
            stepx = float(dx) / float(step)
            stepy = float(dy) / float(step)
            for other_robot in [j for j in self._robots_by_token.values() if j != robot]:
                """:type other_robot: Robot """
                xp, yp = x0, y0
                for i in xrange(int(step) + 1):
                    dist, angle = other_robot.distance_from_point((xp, yp))
                    if dist < 2.0:  # 1 per ogni robot
                        other_robot.take_damage(self._wall_hit_damage)
                        other_robot.block()
                        teta = atan2(dy, dx)
                        xp, yp = other_robot.get_xy()
                        xp -= 2 * cos(teta)
                        yp -= 2 * sin(teta)
                        return xp, yp, self._wall_hit_damage
                    xp += stepx
                    yp += stepy

        # COLLISION WITH WALLS
        if x < 0:
            y = y - dy * x / dx
            x = 0
            return x, y, self._wall_hit_damage
        if x > self._size[0]:
            y = y - dy * (x - self._size[0]) / dx
            x = self._size[0]
            return x, y, self._wall_hit_damage
        if y < 0:
            x = x - dx * y / dy
            y = 0
            return x, y, self._wall_hit_damage
        if y > self._size[1]:
            x = x - dx * (y - self._size[1]) / dy
            y = self._size[1]
            return x, y, self._wall_hit_damage
        return None

    def spawn_missile(self, xy, degree, distance, bullet_speed, bullet_damage, owner=None):
        """
        :type xy: (float, float)
        :type degree: float
        :type distance: float
        :type bullet_speed: float
        :type bullet_damage: float
        :type owner: Robot
        """

        key = uuid.uuid4()
        missile = Missile(self, key, xy, degree, distance, bullet_speed, bullet_damage, owner)
        self._missiles[key] = missile

    def remove_missile(self, missile):
        """
        :type missile: Missile
        """
        del self._missiles[missile]

    def spawn_explosion(self, xy, damage, owner=None):
        key = uuid.uuid4()
        self._explosions[key] = Explosion(self, key, xy, damage, owner)

    def remove_explosion(self, explosion):
        """
        :type explosion: Explosion
        """
        del self._explosions[explosion.key]

    def tick(self, deltatime=0.125):
        self._global_time = self._global_time + deltatime

        # manage missiles
        for m in self._missiles.values():
            assert isinstance(m, Missile)
            m.tick(deltatime)
        # manage explosions
        for e in self._explosions.values():
            assert isinstance(e, Explosion)
            e.tick(deltatime)
        # manage robots
        for r in self.robots.values():
            r.tick(deltatime)

    @staticmethod
    def angle_distance(angle, degree):
        """
        :type angle: float
        :type degree: float

        :param angle:
        :param degree:
        :return:
        TODO document the meaning
        """
        ret = (angle - degree) if angle > degree else (degree - angle)
        if ret > 180.0:
            ret = 360.0 - ret
        return ret


class Missile:
    def __init__(self, board, key, xy, degree, distance, speed, damage, owner=None):
        """
        :type board: Board
        :type key: str
        :type xy: (float, float)
        :type degree: float
        :type distance: float
        :type speed: float
        :type damage: float
        :type owner: Robot
        """
        self._key = key
        self._board = board
        self._owner = owner
        self._x, self._y = xy
        self._degree = degree
        self._distance = distance
        self._speed = speed
        self._damage = damage
        self._space = 0.0

    def tick(self, deltatime):
        dx = self._speed * cos(radians(self._degree)) * deltatime
        dy = self._speed * sin(radians(self._degree)) * deltatime
        self._distance -= self._speed * deltatime
        if self._distance < 1:
            self._board.spawn_explosion((self._x + dx + self._distance * cos(radians(self._degree)),
                                         self._y + dy + self._distance * sin(radians(self._degree))),
                                        self._damage,
                                        self._owner)
            self._board.remove_missile(self)
        self._x += dx
        self._y += dy

    def get_status(self):
        return dict(
            x=self._x,
            y=self._y,
            degree=self._degree,
            distance=self._distance,
            speed=self._speed,
            damage=self._damage
        )


class Explosion:
    """An missile explosion.
    NOTE: a missile is balistic, so it has a precise point of explosion.
    """

    _damage = 0.0
    """:type : float
    The maximum damage of the explosion.
    """

    def get_max_damage(self):
        """
        :return: the maximum damage of the explosio
        :rtype: float
        """
        return self._damage

    def get_perfect_damage_distance(self):
        """
        :return: the distance within an explosion hit perfectly the target, and it is doing 100% of the damage.
        This is near 1, and it is an estimation of robot and missile dimensions.
        :rtype: float
        """
        return 2.0

    def get_max_damage_distance(self):
        """
        :return: the distance after an explosion does 0 damage.
        :rtype: float
        """""
        return 45.0

    def get_initial_relative_damage_of_near_explosion(self):
        """
        :return: the perc of damage of an explosion not hitting the enemy robot. From 0 to 1.
        :rtype: float
        """
        return 0.1

    def get_damage(self, distance):
        """
        :param distance:
        :type distance: float

        :return: the damage according the distance of the target from the explosion.
        :rtype: float
        """

        if distance <= self.get_perfect_damage_distance():
            return self._damage

        if distance >= self.get_max_damage_distance():
            return 0.0

        # calculate a proportional damage
        distance2 = distance - self.get_perfect_damage_distance()
        max_distance2 = self.get_max_damage_distance() - self.get_perfect_damage_distance()
        relative_distance = 1.0 - distance2 / max_distance2

        return self.get_max_damage() * self.get_initial_relative_damage_of_near_explosion() * relative_distance

    def __init__(self, board, key, xy, damage, owner=None):
        """
        :type board: Board
        :type key: str
        :type xy: (float, float)
        :type damage: float
        :type owner: Robot
        """
        self._key = key
        self._owner = owner
        self._board = board
        self._x, self._y = xy
        self._damage = damage
        self._step = 0
        self._explosion_time = 1.0  # sec

    def tick(self, deltatime):
        self._step += 1
        if 1 == self._step:
            # do damage
            for robot in self._board._robots_by_token.values():
                distance, angle = robot.distance_from_point((self._x, self._y))
                total_damage = self.get_damage(distance)

                if total_damage > 0:
                    robot.take_damage(total_damage)
                    if robot.is_dead():
                        self._board.remove_robot(robot)

        elif self._step > 1:
            self._explosion_time -= deltatime
            if self._explosion_time <= 0.0:
                self._board.remove_explosion(self)

    # TODO aggiornare
    def get_status(self):
        return dict(
            x=self._x,
            y=self._y,
            step=self._step,
            damage=self._damage,
            time=self._explosion_time
        )

class Robot(RobotStatus):
    """ A robot in the board.
    """

    def __init__(self, name, token, board, configuration=None):
        """
        :param name: the name of the robot
        :param token: the unique token
        :param configuration: None for using default configurations. A configuration to use otherwise.

        :type name: str
        :type token: str
        :type board: Board
        :type configuration: RobotConfiguration
        """

        self._last_command_executed_at_global_time = 0.0
        self._speed = 0.0
        self._required_speed = 0.0
        self._direction = float(randint(0,359))
        self._fired_new_missile = False
        self._reloading_counter = 0.0
        self._board = board
        self._next_command = None

        # Find an initial position without collisions
        free_position = False
        while not free_position:
            self._pos_x, self._pos_y = float(randint(100, 900)), float(randint(100, 900))
            collision = self._board.detect_collision(self, (1, 1))
            if collision is None:
                free_position = True

        if configuration is None:
            self.set_to_default_configured_strenght()
        else:
            self._configuration = configuration
            self.normalize_to_valid_strenght()

    def set_next_command(self, command):
        """
        Set the command, and register using a new token.

        :type command: RobotCommand
        """
        self._next_command = command
        self._board.remove_robot(self)
        self._token = uuid.uuid4()
        self._board.add_robot(self)

    def get_next_command(self):
        """
        :rtype: RobotCommand
        """
        return self._next_command

    def reset_next_command(self):
        """Signal that the command is applied.
        """
        self._next_command = None

    def max_configurable_strenght(self):
        """
        :rtype: float
        :return: the maximum configurable strenght of the robot,
        that is an estimate of all the characteristicts of the robot like speed, health, acceleration and so on
        """
        return 327

    def get_maximum_configurable_reloading_time(self):
        """
        :rtype: float
        :return: the maximum time a robot can reload the fire.
        IMPORTANT: change the REST API description in case of modification of these values.
        """
        return 6.0

    def get_minimum_configurable_reloading_time(self):
        """
        :rtype: float
        :return: the minimum time a robot can reload the fire
        IMPORTANT: change the REST API description in case of modification of these values.
        """
        return 1.0

    def get_minimum_configurable_bullet_damage(self):
        """
        :rtype: float
        IMPORTANT: change the REST API description in case of modification of these values.
        """
        return 1.0

    def get_maximum_configurable_bullet_damage(self):
        """
        :rtype: float
        IMPORTANT: change the REST API description in case of modification of these values.
        """
        return 20.0

    def get_configured_strenght(self):
        """
        :rtype: float
        :return: from 0 to max_configurable_strenght, for a measure of the strenght of the robot,
        according its speed, acceleration, health and so on.
        TODO adapt this function with experience to a balanced setting
        """

        reloading_time = min(self._configuration._fire_reloading_time, self.get_maximum_configurable_reloading_time())
        reloading_time = max(reloading_time, self.get_minimum_configurable_reloading_time())
        fires_for_second = 1.0 / reloading_time

        bullet_damage = min(self._configuration._bullet_damage, self.get_maximum_configurable_bullet_damage())
        bullet_damage = max(bullet_damage, self.get_minimum_configurable_bullet_damage())

        return sum([
            self._configuration._max_hit_points,
            2 * self._configuration._max_speed,
            2 * self._configuration._max_acceleration,
            -2 * self._configuration._max_decelleration,
            2 * self._configuration._max_sterling_speed,
            0.01 * self._configuration._max_scan_distance,
            0.01 * self._configuration._max_fire_distance,
            0.01 * self._configuration._bullet_speed,
            bullet_damage * 10.0 * fires_for_second,
        ])

    def set_to_default_configured_strenght(self):
        """
        Configure the robot to its default configurations.

        IMPORTANT: change the REST API description in case of modification of these values.
        """

        self._configuration = RobotConfiguration()

        self._configuration._max_hit_points = 100.0
        # TODO non sono sicuro di questo valore

        self._configuration._max_speed = 27.0  # m/s
        self._configuration._max_acceleration = 9.0  # m/s^2
        self._configuration._max_decelleration = -5.0  # m/s^2
        self._configuration._max_sterling_speed = 13.0
        self._configuration._max_scan_distance = 700.0
        self._configuration._max_fire_distance = 700.0
        self._configuration._bullet_speed = 500.0  # m/s
        self._configuration._bullet_damage = 10.0
        self._configuration._fire_reloading_time = 1.0

        self.normalize_to_valid_strenght()

    def configured_strenght_respect_max_configurable(self):
        """
        :return: 1 if the robot is using 100% of maximum configurable strenght,
        0 if it is nothing.
        :rtype: float
        """

        return self.configured_strenght() / self.max_configurable_strenght()

    def normalize_to_valid_strenght(self):
        """
        Reduce or increase strength to a valid value.
        Up to date the only effect is maximizing or minimizing the bullet damage.
        """
        if self.get_configured_strenght() > 1.0:
            self.reduce_to_valid_strength()
        else:
            self.maximize_configured_strength()

    def reduce_to_valid_strength(self):
        """
        Reduce the strength to a valid value.
        Up to date the only effect is reducing the bullet damage.
        :return:
        """
        bullet_damage = self._configuration._bullet_damage
        delta = 0.5
        while self.get_configured_strenght() > 1.0 && bullet_damage >= self.get_minimum_configurable_bullet_damage():
            bullet_damage = bullet_damage - delta
            self._configuration._bullet_damage = bullet_damage

        if bullet_damage < self.get_minimum_configurable_bullet_damage():
            self.set_to_default_configured_strenght()

    def maximize_configured_strength(self):
        """
        Change the current configured_strength for reaching the maximum available.
        Up to date the only effect is in maximizing the bullet damage
        """
        bullet_damage = self._configuration._bullet_damage
        delta = 0.5
        while self.get_configured_strenght() <= 1.0 && bullet_damage <= self.get_maximum_configurable_bullet_damage():
            bullet_damage = bullet_damage + delta
            self._configuration._bullet_damage = bullet_damage

        if bullet_damage >= self.get_maximum_configurable_bullet_damage():
            self.set_to_default_configured_strenght()
        else:
            self._configuration._bullet_damage = bullet_damage - delta

    # TODO adapt
    def get_data(self):
        """
        :return:a dictionary with the status of the robot in a format recognized from the Board viewer tool,
        and from old Rest based API.
        """
        return dict(
            name=self._name,
            max_hit_points=self._max_hit_points,
            required_speed=self._required_speed,
            max_speed=self._max_speed,
            acceleration=self._acceleration,
            decelleration=self._decelleration,
            max_sterling_speed=self._max_sterling_speed,
            max_scan_distance=self._max_scan_distance,
            max_fire_distance=self._max_fire_distance,
            bullet_speed=self._bullet_speed,
            bullet_damage=self._bullet_damage,
            reloading_time=self._reloading_time,
            reloading_counter=self._reloading_counter,
            fired_new_missile=self.fired_new_missile
        )

    # TODO avoid to send commands to dead robot

    def drive(self, degree, speed):
        """
        Change the direction and speed of the robot.
        :type degree: float
        :type speed: float
        """
        degree, speed = float(int(degree) % 360), speed
        if degree != self._heading and self._speed > self._max_sterling_speed:
            # decellerate without changing direction
            self._required_speed = 0.0
        else:
            self._heading = degree
            self._required_speed = min(speed, self._configuration._max_speed)

    def scan(self, direction, semiApertureAngle):
        """
        Activate a scan command.
        :type direction: float
        :type semiApertureAngle: float
        """
        direction, semiApertureAngle = float(int(direction) % 360), float(max(1, int(semiApertureAngle) % 180))

        # TODO sono sicuro sia eseguito nel tempo di simulazione giusto?
        # TODO invocare questo insieme agli altri
        distance = self._board.radar(self, (self._pos_x, self._pos_y), self._configuration._max_scan_distance, direction, semiApertureAngle)

        scan = new ScanStatus()
        scan.setDirection(direction)
        scan.setSemiApertureAngle(semiApertureAngle)
        scan.setDistance(distance)
        self._scan_status = scan

    def no_scan(self):
        self._scan_status = None

    def cannon(self, degree, distance):
        """
        Activate cannon command if the robot can fire a missile. Nothing otherwise.
        :type degree: float
        :type distance: float
        :param degree:
        :param distance:
        """
        if self._reloading is False:
            degree, distance = float(int(degree) % 360), min(distance, self._configuration._max_fire_distance)

            # TODO vedere questo
            self._board.spawn_missile((self._pos_x, self._pos_y), degree, distance, self._configuration._bullet_speed, self._configuration._bullet_damage,
                                      self)
            self._reloading = True
            self._reloading_counter = 0.0

            self._fired_new_missile = True
        else:
            self._fired_new_missile = False

    def no_cannon(self):
        self._fired_new_missile = False

    def distance_from_point(self, xy):
        """
        :type xy: (float, float)
        :rtype: float
        :param xy: point
        :rtype: (float, float)
        :return: distance, angle
        """
        dx = (xy[0] - self._pos_x)
        dy = (xy[1] - self._pos_y)
        dist = (dx ** 2 + dy ** 2) ** 0.5
        rads = atan2(dy, dx)
        angle = float(degrees(rads) % 360)
        return dist, angle

    def distance(self, enemy):
        """
        :type enemy: Robot
        :rtype: (float, float)
        :return: distance, angle
        """
        dist, angle = enemy.distance((self._pos_x, self._pos_y))
        angle = float(int(angle + 180.0) % 360)
        return dist, angle

    def block(self):
        """ Block immediately the robot.
        Call only in case of death or hit of a wall, because immediate block without deceleration can be cheating...
        """
        self._speed = 0.0
        self._required_speed = 0.0

    def tick(self, deltatime):
        """Update the status of the robot for a simulation tick.

        :type deltatime: float
        """


        # RELOADING
        if self._reloading:
            self._reloading_counter += deltatime
            if self._reloading_counter >= self._configuration._reloading_time:
                self._reloading = False
                self._reloading_counter = 0.0

        # SPEED calculation with accelleration/decelleration limit
        if self._speed <= self._required_speed:
            delta = accel = min(self._configuration._max_acceleration,
                                (self._required_speed - self._speed) / deltatime)
        else:
            delta = decell = max(self._configuration._max_decelleration,
                                 (self._required_speed - self._speed) / deltatime)

        # MOVEMENT
        movement = self._speed * deltatime + 0.5 * delta * deltatime ** 2
        dx = movement * cos(radians(self._heading))
        dy = movement * sin(radians(self._heading))
        self._pos_x += dx
        self._pos_y += dy

        # UPDATE velocity
        self._speed += delta * deltatime

        # COLLISION
        collision = self._board.detect_collision(self, (dx, dy))
        if collision is not None:
            self.block()
            self._pos_x, self._pos_y = collision[:2]
            self.take_damage(collision[2])

    def take_damage(self, hp):
        """ Inflict a damage to the robot.
        :type hp: float
        :param hp:
        """
        self._health -= hp
        if self._health <= 0:
            self.block()
            self._health = 0
            self._is_dead = True

    def get_xy(self):
        """
        :rtype: (float, float)
        """
        return self._pos_x, self._pos_y

