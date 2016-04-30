<?php
/**
 * RobotStatus
 *
 * PHP version 5
 *
 * @category Class
 * @package  Swagger\Client
 * @author   http://github.com/swagger-api/swagger-codegen
 * @license  http://www.apache.org/licenses/LICENSE-2.0 Apache Licene v2
 * @link     https://github.com/swagger-api/swagger-codegen
 */
/**
 *  Copyright 2016 SmartBear Software
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
/**
 * NOTE: This class is auto generated by the swagger code generator program.
 * https://github.com/swagger-api/swagger-codegen
 * Do not edit the class manually.
 */

namespace Swagger\Client\Model;

use \ArrayAccess;
/**
 * RobotStatus Class Doc Comment
 *
 * @category    Class
 * @description The current status of the robot.
 * @package     Swagger\Client
 * @author      http://github.com/swagger-api/swagger-codegen
 * @license     http://www.apache.org/licenses/LICENSE-2.0 Apache Licene v2
 * @link        https://github.com/swagger-api/swagger-codegen
 */
class RobotStatus implements ArrayAccess
{
    /**
      * Array of property to type mappings. Used for (de)serialization 
      * @var string[]
      */
    static $swaggerTypes = array(
        'name' => 'string',
        'token' => 'string',
        'configuration' => '\Swagger\Client\Model\RobotConfiguration',
        'simulation_time' => 'float',
        'time_tick' => 'float',
        'real_time_tick' => 'float',
        'health' => 'float',
        'is_dead' => 'bool',
        'is_winner' => 'bool',
        'is_well_specified_robot' => 'bool',
        'direction' => 'float',
        'speed' => 'float',
        'pos_x' => 'float',
        'pos_y' => 'float',
        'cannon_reloading_time' => 'float',
        'fired_new_missile' => 'bool',
        'scan_status' => '\Swagger\Client\Model\ScanStatus'
    );
  
    static function swaggerTypes() {
        return self::$swaggerTypes;
    }

    /** 
      * Array of attributes where the key is the local name, and the value is the original name
      * @var string[] 
      */
    static $attributeMap = array(
        'name' => 'name',
        'token' => 'token',
        'configuration' => 'configuration',
        'simulation_time' => 'simulationTime',
        'time_tick' => 'timeTick',
        'real_time_tick' => 'realTimeTick',
        'health' => 'health',
        'is_dead' => 'isDead',
        'is_winner' => 'isWinner',
        'is_well_specified_robot' => 'isWellSpecifiedRobot',
        'direction' => 'direction',
        'speed' => 'speed',
        'pos_x' => 'posX',
        'pos_y' => 'posY',
        'cannon_reloading_time' => 'cannonReloadingTime',
        'fired_new_missile' => 'firedNewMissile',
        'scan_status' => 'scanStatus'
    );
  
    static function attributeMap() {
        return self::$attributeMap;
    }

    /**
      * Array of attributes to setter functions (for deserialization of responses)
      * @var string[]
      */
    static $setters = array(
        'name' => 'setName',
        'token' => 'setToken',
        'configuration' => 'setConfiguration',
        'simulation_time' => 'setSimulationTime',
        'time_tick' => 'setTimeTick',
        'real_time_tick' => 'setRealTimeTick',
        'health' => 'setHealth',
        'is_dead' => 'setIsDead',
        'is_winner' => 'setIsWinner',
        'is_well_specified_robot' => 'setIsWellSpecifiedRobot',
        'direction' => 'setDirection',
        'speed' => 'setSpeed',
        'pos_x' => 'setPosX',
        'pos_y' => 'setPosY',
        'cannon_reloading_time' => 'setCannonReloadingTime',
        'fired_new_missile' => 'setFiredNewMissile',
        'scan_status' => 'setScanStatus'
    );
  
    static function setters() {
        return self::$setters;
    }

    /**
      * Array of attributes to getter functions (for serialization of requests)
      * @var string[]
      */
    static $getters = array(
        'name' => 'getName',
        'token' => 'getToken',
        'configuration' => 'getConfiguration',
        'simulation_time' => 'getSimulationTime',
        'time_tick' => 'getTimeTick',
        'real_time_tick' => 'getRealTimeTick',
        'health' => 'getHealth',
        'is_dead' => 'getIsDead',
        'is_winner' => 'getIsWinner',
        'is_well_specified_robot' => 'getIsWellSpecifiedRobot',
        'direction' => 'getDirection',
        'speed' => 'getSpeed',
        'pos_x' => 'getPosX',
        'pos_y' => 'getPosY',
        'cannon_reloading_time' => 'getCannonReloadingTime',
        'fired_new_missile' => 'getFiredNewMissile',
        'scan_status' => 'getScanStatus'
    );
  
    static function getters() {
        return self::$getters;
    }

    
    /**
      * $name 
      * @var string
      */
    protected $name;
    
    /**
      * $token A unique token, that changes after a command is sent from a robot to the server.\nIt is used for recognizing a robot, and chaining the commands, because a robot can not issue another command, before it receive an answer from the server, with the next token to use.\n
      * @var string
      */
    protected $token;
    
    /**
      * $configuration 
      * @var \Swagger\Client\Model\RobotConfiguration
      */
    protected $configuration;
    
    /**
      * $simulation_time The current simulation time.
      * @var float
      */
    protected $simulation_time;
    
    /**
      * $time_tick The next command will be executed at simulationTime + this value. Usually it is a constant value for all the course of the simulation.
      * @var float
      */
    protected $time_tick;
    
    /**
      * $real_time_tick The time in seconds, the system waits before processing the next request from remote robots.\nWith slow nework connections this value should be higher, because otherwise some remote robots could miss some game turns.\nNOTE: this time differs from timeIncrement, because timeIncrement is the simulation time that pass between two robots commands.\n
      * @var float
      */
    protected $real_time_tick;
    
    /**
      * $health The health of a robot. 0 when a robot is dead (completely destroyed).
      * @var float
      */
    protected $health;
    
    /**
      * $is_dead 
      * @var bool
      */
    protected $is_dead;
    
    /**
      * $is_winner 
      * @var bool
      */
    protected $is_winner;
    
    /**
      * $is_well_specified_robot true if the robot creation params respect the constraints.
      * @var bool
      */
    protected $is_well_specified_robot;
    
    /**
      * $direction Direction expressed in degrees. 0 degree is EAST, 90 degree is NORTH, 180 degree is WEST, 279 degree is SOUTH
      * @var float
      */
    protected $direction;
    
    /**
      * $speed 
      * @var float
      */
    protected $speed;
    
    /**
      * $pos_x 
      * @var float
      */
    protected $pos_x;
    
    /**
      * $pos_y 
      * @var float
      */
    protected $pos_y;
    
    /**
      * $cannon_reloading_time 0 if the robot can fire immediately, the remaining time it must wait otherwise.
      * @var float
      */
    protected $cannon_reloading_time;
    
    /**
      * $fired_new_missile True if the robot in last command fired a missile.
      * @var bool
      */
    protected $fired_new_missile;
    
    /**
      * $scan_status 
      * @var \Swagger\Client\Model\ScanStatus
      */
    protected $scan_status;
    

    /**
     * Constructor
     * @param mixed[] $data Associated array of property value initalizing the model
     */
    public function __construct(array $data = null)
    {
        
        if ($data != null) {
            $this->name = $data["name"];
            $this->token = $data["token"];
            $this->configuration = $data["configuration"];
            $this->simulation_time = $data["simulation_time"];
            $this->time_tick = $data["time_tick"];
            $this->real_time_tick = $data["real_time_tick"];
            $this->health = $data["health"];
            $this->is_dead = $data["is_dead"];
            $this->is_winner = $data["is_winner"];
            $this->is_well_specified_robot = $data["is_well_specified_robot"];
            $this->direction = $data["direction"];
            $this->speed = $data["speed"];
            $this->pos_x = $data["pos_x"];
            $this->pos_y = $data["pos_y"];
            $this->cannon_reloading_time = $data["cannon_reloading_time"];
            $this->fired_new_missile = $data["fired_new_missile"];
            $this->scan_status = $data["scan_status"];
        }
    }
    
    /**
     * Gets name
     * @return string
     */
    public function getName()
    {
        return $this->name;
    }
  
    /**
     * Sets name
     * @param string $name 
     * @return $this
     */
    public function setName($name)
    {
        
        $this->name = $name;
        return $this;
    }
    
    /**
     * Gets token
     * @return string
     */
    public function getToken()
    {
        return $this->token;
    }
  
    /**
     * Sets token
     * @param string $token A unique token, that changes after a command is sent from a robot to the server.\nIt is used for recognizing a robot, and chaining the commands, because a robot can not issue another command, before it receive an answer from the server, with the next token to use.\n
     * @return $this
     */
    public function setToken($token)
    {
        
        $this->token = $token;
        return $this;
    }
    
    /**
     * Gets configuration
     * @return \Swagger\Client\Model\RobotConfiguration
     */
    public function getConfiguration()
    {
        return $this->configuration;
    }
  
    /**
     * Sets configuration
     * @param \Swagger\Client\Model\RobotConfiguration $configuration 
     * @return $this
     */
    public function setConfiguration($configuration)
    {
        
        $this->configuration = $configuration;
        return $this;
    }
    
    /**
     * Gets simulation_time
     * @return float
     */
    public function getSimulationTime()
    {
        return $this->simulation_time;
    }
  
    /**
     * Sets simulation_time
     * @param float $simulation_time The current simulation time.
     * @return $this
     */
    public function setSimulationTime($simulation_time)
    {
        
        $this->simulation_time = $simulation_time;
        return $this;
    }
    
    /**
     * Gets time_tick
     * @return float
     */
    public function getTimeTick()
    {
        return $this->time_tick;
    }
  
    /**
     * Sets time_tick
     * @param float $time_tick The next command will be executed at simulationTime + this value. Usually it is a constant value for all the course of the simulation.
     * @return $this
     */
    public function setTimeTick($time_tick)
    {
        
        $this->time_tick = $time_tick;
        return $this;
    }
    
    /**
     * Gets real_time_tick
     * @return float
     */
    public function getRealTimeTick()
    {
        return $this->real_time_tick;
    }
  
    /**
     * Sets real_time_tick
     * @param float $real_time_tick The time in seconds, the system waits before processing the next request from remote robots.\nWith slow nework connections this value should be higher, because otherwise some remote robots could miss some game turns.\nNOTE: this time differs from timeIncrement, because timeIncrement is the simulation time that pass between two robots commands.\n
     * @return $this
     */
    public function setRealTimeTick($real_time_tick)
    {
        
        $this->real_time_tick = $real_time_tick;
        return $this;
    }
    
    /**
     * Gets health
     * @return float
     */
    public function getHealth()
    {
        return $this->health;
    }
  
    /**
     * Sets health
     * @param float $health The health of a robot. 0 when a robot is dead (completely destroyed).
     * @return $this
     */
    public function setHealth($health)
    {
        
        $this->health = $health;
        return $this;
    }
    
    /**
     * Gets is_dead
     * @return bool
     */
    public function getIsDead()
    {
        return $this->is_dead;
    }
  
    /**
     * Sets is_dead
     * @param bool $is_dead 
     * @return $this
     */
    public function setIsDead($is_dead)
    {
        
        $this->is_dead = $is_dead;
        return $this;
    }
    
    /**
     * Gets is_winner
     * @return bool
     */
    public function getIsWinner()
    {
        return $this->is_winner;
    }
  
    /**
     * Sets is_winner
     * @param bool $is_winner 
     * @return $this
     */
    public function setIsWinner($is_winner)
    {
        
        $this->is_winner = $is_winner;
        return $this;
    }
    
    /**
     * Gets is_well_specified_robot
     * @return bool
     */
    public function getIsWellSpecifiedRobot()
    {
        return $this->is_well_specified_robot;
    }
  
    /**
     * Sets is_well_specified_robot
     * @param bool $is_well_specified_robot true if the robot creation params respect the constraints.
     * @return $this
     */
    public function setIsWellSpecifiedRobot($is_well_specified_robot)
    {
        
        $this->is_well_specified_robot = $is_well_specified_robot;
        return $this;
    }
    
    /**
     * Gets direction
     * @return float
     */
    public function getDirection()
    {
        return $this->direction;
    }
  
    /**
     * Sets direction
     * @param float $direction Direction expressed in degrees. 0 degree is EAST, 90 degree is NORTH, 180 degree is WEST, 279 degree is SOUTH
     * @return $this
     */
    public function setDirection($direction)
    {
        
        $this->direction = $direction;
        return $this;
    }
    
    /**
     * Gets speed
     * @return float
     */
    public function getSpeed()
    {
        return $this->speed;
    }
  
    /**
     * Sets speed
     * @param float $speed 
     * @return $this
     */
    public function setSpeed($speed)
    {
        
        $this->speed = $speed;
        return $this;
    }
    
    /**
     * Gets pos_x
     * @return float
     */
    public function getPosX()
    {
        return $this->pos_x;
    }
  
    /**
     * Sets pos_x
     * @param float $pos_x 
     * @return $this
     */
    public function setPosX($pos_x)
    {
        
        $this->pos_x = $pos_x;
        return $this;
    }
    
    /**
     * Gets pos_y
     * @return float
     */
    public function getPosY()
    {
        return $this->pos_y;
    }
  
    /**
     * Sets pos_y
     * @param float $pos_y 
     * @return $this
     */
    public function setPosY($pos_y)
    {
        
        $this->pos_y = $pos_y;
        return $this;
    }
    
    /**
     * Gets cannon_reloading_time
     * @return float
     */
    public function getCannonReloadingTime()
    {
        return $this->cannon_reloading_time;
    }
  
    /**
     * Sets cannon_reloading_time
     * @param float $cannon_reloading_time 0 if the robot can fire immediately, the remaining time it must wait otherwise.
     * @return $this
     */
    public function setCannonReloadingTime($cannon_reloading_time)
    {
        
        $this->cannon_reloading_time = $cannon_reloading_time;
        return $this;
    }
    
    /**
     * Gets fired_new_missile
     * @return bool
     */
    public function getFiredNewMissile()
    {
        return $this->fired_new_missile;
    }
  
    /**
     * Sets fired_new_missile
     * @param bool $fired_new_missile True if the robot in last command fired a missile.
     * @return $this
     */
    public function setFiredNewMissile($fired_new_missile)
    {
        
        $this->fired_new_missile = $fired_new_missile;
        return $this;
    }
    
    /**
     * Gets scan_status
     * @return \Swagger\Client\Model\ScanStatus
     */
    public function getScanStatus()
    {
        return $this->scan_status;
    }
  
    /**
     * Sets scan_status
     * @param \Swagger\Client\Model\ScanStatus $scan_status 
     * @return $this
     */
    public function setScanStatus($scan_status)
    {
        
        $this->scan_status = $scan_status;
        return $this;
    }
    
    /**
     * Returns true if offset exists. False otherwise.
     * @param  integer $offset Offset 
     * @return boolean
     */
    public function offsetExists($offset)
    {
        return isset($this->$offset);
    }
  
    /**
     * Gets offset.
     * @param  integer $offset Offset 
     * @return mixed 
     */
    public function offsetGet($offset)
    {
        return $this->$offset;
    }
  
    /**
     * Sets value based on offset.
     * @param  integer $offset Offset 
     * @param  mixed   $value  Value to be set
     * @return void
     */
    public function offsetSet($offset, $value)
    {
        $this->$offset = $value;
    }
  
    /**
     * Unsets offset.
     * @param  integer $offset Offset 
     * @return void
     */
    public function offsetUnset($offset)
    {
        unset($this->$offset);
    }
  
    /**
     * Gets the string presentation of the object
     * @return string
     */
    public function __toString()
    {
        if (defined('JSON_PRETTY_PRINT')) {
            return json_encode(\Swagger\Client\ObjectSerializer::sanitizeForSerialization($this), JSON_PRETTY_PRINT);
        } else {
            return json_encode(\Swagger\Client\ObjectSerializer::sanitizeForSerialization($this));
        }
    }
}
