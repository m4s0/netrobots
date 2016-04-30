<?php
/**
 * RobotConfiguration
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
 * RobotConfiguration Class Doc Comment
 *
 * @category    Class
 * @description Configurations of the robot.\nConfigurations can be sent during robot creation, or can be returned after robot creation.\n
 * @package     Swagger\Client
 * @author      http://github.com/swagger-api/swagger-codegen
 * @license     http://www.apache.org/licenses/LICENSE-2.0 Apache Licene v2
 * @link        https://github.com/swagger-api/swagger-codegen
 */
class RobotConfiguration implements ArrayAccess
{
    /**
      * Array of property to type mappings. Used for (de)serialization 
      * @var string[]
      */
    static $swaggerTypes = array(
        'name' => 'string',
        'max_hit_points' => 'float',
        'max_speed' => 'float',
        'acceleration' => 'float',
        'decelleration' => 'float',
        'max_sterling_speed' => 'float',
        'max_scan_distance' => 'float',
        'max_fire_distance' => 'float',
        'bullet_speed' => 'float',
        'bullet_damage' => 'float',
        'fire_reloading_time' => 'float'
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
        'max_hit_points' => 'maxHitPoints',
        'max_speed' => 'maxSpeed',
        'acceleration' => 'acceleration',
        'decelleration' => 'decelleration',
        'max_sterling_speed' => 'maxSterlingSpeed',
        'max_scan_distance' => 'maxScanDistance',
        'max_fire_distance' => 'maxFireDistance',
        'bullet_speed' => 'bulletSpeed',
        'bullet_damage' => 'bulletDamage',
        'fire_reloading_time' => 'fireReloadingTime'
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
        'max_hit_points' => 'setMaxHitPoints',
        'max_speed' => 'setMaxSpeed',
        'acceleration' => 'setAcceleration',
        'decelleration' => 'setDecelleration',
        'max_sterling_speed' => 'setMaxSterlingSpeed',
        'max_scan_distance' => 'setMaxScanDistance',
        'max_fire_distance' => 'setMaxFireDistance',
        'bullet_speed' => 'setBulletSpeed',
        'bullet_damage' => 'setBulletDamage',
        'fire_reloading_time' => 'setFireReloadingTime'
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
        'max_hit_points' => 'getMaxHitPoints',
        'max_speed' => 'getMaxSpeed',
        'acceleration' => 'getAcceleration',
        'decelleration' => 'getDecelleration',
        'max_sterling_speed' => 'getMaxSterlingSpeed',
        'max_scan_distance' => 'getMaxScanDistance',
        'max_fire_distance' => 'getMaxFireDistance',
        'bullet_speed' => 'getBulletSpeed',
        'bullet_damage' => 'getBulletDamage',
        'fire_reloading_time' => 'getFireReloadingTime'
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
      * $max_hit_points The initial health of the robot. Default value: 100.0
      * @var float
      */
    protected $max_hit_points;
    
    /**
      * $max_speed Max speed of the robot. Default value: 27.0 m/s
      * @var float
      */
    protected $max_speed;
    
    /**
      * $acceleration Acceleration of the robot. Default value 9.0 m/s^2
      * @var float
      */
    protected $acceleration;
    
    /**
      * $decelleration The max deceleration in case of reduction of the speed. A negative number. Default value -5.0 m/s^2
      * @var float
      */
    protected $decelleration;
    
    /**
      * $max_sterling_speed The maximim speed at which a robot can change direction.  Default value 13.0 m/s.\nIf the robot changes direction when it has a speed greater than this value, it start decelerating without changing direction, until it does not reach the steerling speed.\n
      * @var float
      */
    protected $max_sterling_speed;
    
    /**
      * $max_scan_distance The max distance the robot can identify targets. Default value: 700 m/s.
      * @var float
      */
    protected $max_scan_distance;
    
    /**
      * $max_fire_distance The max distance a bullet can reach. Default value: 700 m/s.
      * @var float
      */
    protected $max_fire_distance;
    
    /**
      * $bullet_speed The speed of a fired bullet. Default value: 500 m/s.
      * @var float
      */
    protected $bullet_speed;
    
    /**
      * $bullet_damage The max health damage a bullet can inflict when it reach exactly the target.\nDefault value: 10.0.\nActually max allowed value is 20 and minimum allowed value is 1.\nThe bullet hit exactly a target in case the explosion is within a certain distance, actually 2 meters.\nThe bullet can make a limited damage if it explodes near the target.\nThe limited damage decrease linearly from 10% to 0%, from 2 meters to 45 meters distance from the target.\nWhen you configure a robot, it is associated a strenght using an heuristic, based on robot configured characteristics.\nThen the bulletDamage is increased until it does not reach the maximum value respecting the maximum allowed robot strenght.\n
      * @var float
      */
    protected $bullet_damage;
    
    /**
      * $fire_reloading_time How many seconds the robot must wait before firing another missile. Deafault value: 1.0 s\nActually this value must be within 1.0 s and 6.0 s\n
      * @var float
      */
    protected $fire_reloading_time;
    

    /**
     * Constructor
     * @param mixed[] $data Associated array of property value initalizing the model
     */
    public function __construct(array $data = null)
    {
        
        if ($data != null) {
            $this->name = $data["name"];
            $this->max_hit_points = $data["max_hit_points"];
            $this->max_speed = $data["max_speed"];
            $this->acceleration = $data["acceleration"];
            $this->decelleration = $data["decelleration"];
            $this->max_sterling_speed = $data["max_sterling_speed"];
            $this->max_scan_distance = $data["max_scan_distance"];
            $this->max_fire_distance = $data["max_fire_distance"];
            $this->bullet_speed = $data["bullet_speed"];
            $this->bullet_damage = $data["bullet_damage"];
            $this->fire_reloading_time = $data["fire_reloading_time"];
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
     * Gets max_hit_points
     * @return float
     */
    public function getMaxHitPoints()
    {
        return $this->max_hit_points;
    }
  
    /**
     * Sets max_hit_points
     * @param float $max_hit_points The initial health of the robot. Default value: 100.0
     * @return $this
     */
    public function setMaxHitPoints($max_hit_points)
    {
        
        $this->max_hit_points = $max_hit_points;
        return $this;
    }
    
    /**
     * Gets max_speed
     * @return float
     */
    public function getMaxSpeed()
    {
        return $this->max_speed;
    }
  
    /**
     * Sets max_speed
     * @param float $max_speed Max speed of the robot. Default value: 27.0 m/s
     * @return $this
     */
    public function setMaxSpeed($max_speed)
    {
        
        $this->max_speed = $max_speed;
        return $this;
    }
    
    /**
     * Gets acceleration
     * @return float
     */
    public function getAcceleration()
    {
        return $this->acceleration;
    }
  
    /**
     * Sets acceleration
     * @param float $acceleration Acceleration of the robot. Default value 9.0 m/s^2
     * @return $this
     */
    public function setAcceleration($acceleration)
    {
        
        $this->acceleration = $acceleration;
        return $this;
    }
    
    /**
     * Gets decelleration
     * @return float
     */
    public function getDecelleration()
    {
        return $this->decelleration;
    }
  
    /**
     * Sets decelleration
     * @param float $decelleration The max deceleration in case of reduction of the speed. A negative number. Default value -5.0 m/s^2
     * @return $this
     */
    public function setDecelleration($decelleration)
    {
        
        $this->decelleration = $decelleration;
        return $this;
    }
    
    /**
     * Gets max_sterling_speed
     * @return float
     */
    public function getMaxSterlingSpeed()
    {
        return $this->max_sterling_speed;
    }
  
    /**
     * Sets max_sterling_speed
     * @param float $max_sterling_speed The maximim speed at which a robot can change direction.  Default value 13.0 m/s.\nIf the robot changes direction when it has a speed greater than this value, it start decelerating without changing direction, until it does not reach the steerling speed.\n
     * @return $this
     */
    public function setMaxSterlingSpeed($max_sterling_speed)
    {
        
        $this->max_sterling_speed = $max_sterling_speed;
        return $this;
    }
    
    /**
     * Gets max_scan_distance
     * @return float
     */
    public function getMaxScanDistance()
    {
        return $this->max_scan_distance;
    }
  
    /**
     * Sets max_scan_distance
     * @param float $max_scan_distance The max distance the robot can identify targets. Default value: 700 m/s.
     * @return $this
     */
    public function setMaxScanDistance($max_scan_distance)
    {
        
        $this->max_scan_distance = $max_scan_distance;
        return $this;
    }
    
    /**
     * Gets max_fire_distance
     * @return float
     */
    public function getMaxFireDistance()
    {
        return $this->max_fire_distance;
    }
  
    /**
     * Sets max_fire_distance
     * @param float $max_fire_distance The max distance a bullet can reach. Default value: 700 m/s.
     * @return $this
     */
    public function setMaxFireDistance($max_fire_distance)
    {
        
        $this->max_fire_distance = $max_fire_distance;
        return $this;
    }
    
    /**
     * Gets bullet_speed
     * @return float
     */
    public function getBulletSpeed()
    {
        return $this->bullet_speed;
    }
  
    /**
     * Sets bullet_speed
     * @param float $bullet_speed The speed of a fired bullet. Default value: 500 m/s.
     * @return $this
     */
    public function setBulletSpeed($bullet_speed)
    {
        
        $this->bullet_speed = $bullet_speed;
        return $this;
    }
    
    /**
     * Gets bullet_damage
     * @return float
     */
    public function getBulletDamage()
    {
        return $this->bullet_damage;
    }
  
    /**
     * Sets bullet_damage
     * @param float $bullet_damage The max health damage a bullet can inflict when it reach exactly the target.\nDefault value: 10.0.\nActually max allowed value is 20 and minimum allowed value is 1.\nThe bullet hit exactly a target in case the explosion is within a certain distance, actually 2 meters.\nThe bullet can make a limited damage if it explodes near the target.\nThe limited damage decrease linearly from 10% to 0%, from 2 meters to 45 meters distance from the target.\nWhen you configure a robot, it is associated a strenght using an heuristic, based on robot configured characteristics.\nThen the bulletDamage is increased until it does not reach the maximum value respecting the maximum allowed robot strenght.\n
     * @return $this
     */
    public function setBulletDamage($bullet_damage)
    {
        
        $this->bullet_damage = $bullet_damage;
        return $this;
    }
    
    /**
     * Gets fire_reloading_time
     * @return float
     */
    public function getFireReloadingTime()
    {
        return $this->fire_reloading_time;
    }
  
    /**
     * Sets fire_reloading_time
     * @param float $fire_reloading_time How many seconds the robot must wait before firing another missile. Deafault value: 1.0 s\nActually this value must be within 1.0 s and 6.0 s\n
     * @return $this
     */
    public function setFireReloadingTime($fire_reloading_time)
    {
        
        $this->fire_reloading_time = $fire_reloading_time;
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
