(function(root, factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
    define(['../ApiClient', './RobotConfiguration', './ScanStatus'], factory);
  } else if (typeof module === 'object' && module.exports) {
    // CommonJS-like environments that support module.exports, like Node.
    module.exports = factory(require('../ApiClient'), require('./RobotConfiguration'), require('./ScanStatus'));
  } else {
    // Browser globals (root is window)
    if (!root.NetRobotsApi) {
      root.NetRobotsApi = {};
    }
    root.NetRobotsApi.RobotStatus = factory(root.NetRobotsApi.ApiClient, root.NetRobotsApi.RobotConfiguration, root.NetRobotsApi.ScanStatus);
  }
}(this, function(ApiClient, RobotConfiguration, ScanStatus) {
  'use strict';

  /**
   * The RobotStatus model module.
   * @module model/RobotStatus
   * @version 2.0
   */

  /**
   * Constructs a new <code>RobotStatus</code>.
   * The current status of the robot.
   * @alias module:model/RobotStatus
   * @class
   * @param name
   * @param token
   * @param configuration
   * @param simulationTime
   * @param timeTick
   * @param realTimeTick
   * @param health
   * @param isDead
   * @param isWinner
   * @param isWellSpecifiedRobot
   * @param direction
   * @param speed
   * @param posX
   * @param posY
   * @param cannonReloadingTime
   * @param firedNewMissile
   */
  var exports = function(name, token, configuration, simulationTime, timeTick, realTimeTick, health, isDead, isWinner, isWellSpecifiedRobot, direction, speed, posX, posY, cannonReloadingTime, firedNewMissile) {

    this['name'] = name;
    this['token'] = token;
    this['configuration'] = configuration;
    this['simulationTime'] = simulationTime;
    this['timeTick'] = timeTick;
    this['realTimeTick'] = realTimeTick;
    this['health'] = health;
    this['isDead'] = isDead;
    this['isWinner'] = isWinner;
    this['isWellSpecifiedRobot'] = isWellSpecifiedRobot;
    this['direction'] = direction;
    this['speed'] = speed;
    this['posX'] = posX;
    this['posY'] = posY;
    this['cannonReloadingTime'] = cannonReloadingTime;
    this['firedNewMissile'] = firedNewMissile;

  };

  /**
   * Constructs a <code>RobotStatus</code> from a plain JavaScript object, optionally creating a new instance.
   * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
   * @param {Object} data The plain JavaScript object bearing properties of interest.
   * @param {module:model/RobotStatus} obj Optional instance to populate.
   * @return {module:model/RobotStatus} The populated <code>RobotStatus</code> instance.
   */
  exports.constructFromObject = function(data, obj) {
    if (data) { 
      obj = obj || new exports();

      if (data.hasOwnProperty('name')) {
        obj['name'] = ApiClient.convertToType(data['name'], 'String');
      }
      if (data.hasOwnProperty('token')) {
        obj['token'] = ApiClient.convertToType(data['token'], 'String');
      }
      if (data.hasOwnProperty('configuration')) {
        obj['configuration'] = RobotConfiguration.constructFromObject(data['configuration']);
      }
      if (data.hasOwnProperty('simulationTime')) {
        obj['simulationTime'] = ApiClient.convertToType(data['simulationTime'], 'Number');
      }
      if (data.hasOwnProperty('timeTick')) {
        obj['timeTick'] = ApiClient.convertToType(data['timeTick'], 'Number');
      }
      if (data.hasOwnProperty('realTimeTick')) {
        obj['realTimeTick'] = ApiClient.convertToType(data['realTimeTick'], 'Number');
      }
      if (data.hasOwnProperty('health')) {
        obj['health'] = ApiClient.convertToType(data['health'], 'Number');
      }
      if (data.hasOwnProperty('isDead')) {
        obj['isDead'] = ApiClient.convertToType(data['isDead'], 'Boolean');
      }
      if (data.hasOwnProperty('isWinner')) {
        obj['isWinner'] = ApiClient.convertToType(data['isWinner'], 'Boolean');
      }
      if (data.hasOwnProperty('isWellSpecifiedRobot')) {
        obj['isWellSpecifiedRobot'] = ApiClient.convertToType(data['isWellSpecifiedRobot'], 'Boolean');
      }
      if (data.hasOwnProperty('direction')) {
        obj['direction'] = ApiClient.convertToType(data['direction'], 'Number');
      }
      if (data.hasOwnProperty('speed')) {
        obj['speed'] = ApiClient.convertToType(data['speed'], 'Number');
      }
      if (data.hasOwnProperty('posX')) {
        obj['posX'] = ApiClient.convertToType(data['posX'], 'Number');
      }
      if (data.hasOwnProperty('posY')) {
        obj['posY'] = ApiClient.convertToType(data['posY'], 'Number');
      }
      if (data.hasOwnProperty('cannonReloadingTime')) {
        obj['cannonReloadingTime'] = ApiClient.convertToType(data['cannonReloadingTime'], 'Number');
      }
      if (data.hasOwnProperty('firedNewMissile')) {
        obj['firedNewMissile'] = ApiClient.convertToType(data['firedNewMissile'], 'Boolean');
      }
      if (data.hasOwnProperty('scanStatus')) {
        obj['scanStatus'] = ScanStatus.constructFromObject(data['scanStatus']);
      }
    }
    return obj;
  }


  /**
   * @member {String} name
   */
  exports.prototype['name'] = undefined;

  /**
   * A unique token, that changes after a command is sent from a robot to the server.\nIt is used for recognizing a robot, and chaining the commands, because a robot can not issue another command, before it receive an answer from the server, with the next token to use.\n
   * @member {String} token
   */
  exports.prototype['token'] = undefined;

  /**
   * @member {module:model/RobotConfiguration} configuration
   */
  exports.prototype['configuration'] = undefined;

  /**
   * The current simulation time.
   * @member {Number} simulationTime
   */
  exports.prototype['simulationTime'] = undefined;

  /**
   * The next command will be executed at simulationTime + this value. Usually it is a constant value for all the course of the simulation.
   * @member {Number} timeTick
   */
  exports.prototype['timeTick'] = undefined;

  /**
   * The time in seconds, the system waits before processing the next request from remote robots.\nWith slow nework connections this value should be higher, because otherwise some remote robots could miss some game turns.\nNOTE: this time differs from timeIncrement, because timeIncrement is the simulation time that pass between two robots commands.\n
   * @member {Number} realTimeTick
   */
  exports.prototype['realTimeTick'] = undefined;

  /**
   * The health of a robot. 0 when a robot is dead (completely destroyed).
   * @member {Number} health
   */
  exports.prototype['health'] = undefined;

  /**
   * @member {Boolean} isDead
   */
  exports.prototype['isDead'] = undefined;

  /**
   * @member {Boolean} isWinner
   */
  exports.prototype['isWinner'] = undefined;

  /**
   * true if the robot creation params respect the constraints.
   * @member {Boolean} isWellSpecifiedRobot
   */
  exports.prototype['isWellSpecifiedRobot'] = undefined;

  /**
   * Direction expressed in degrees. 0 degree is EAST, 90 degree is NORTH, 180 degree is WEST, 279 degree is SOUTH
   * @member {Number} direction
   */
  exports.prototype['direction'] = undefined;

  /**
   * @member {Number} speed
   */
  exports.prototype['speed'] = undefined;

  /**
   * @member {Number} posX
   */
  exports.prototype['posX'] = undefined;

  /**
   * @member {Number} posY
   */
  exports.prototype['posY'] = undefined;

  /**
   * 0 if the robot can fire immediately, the remaining time it must wait otherwise.
   * @member {Number} cannonReloadingTime
   */
  exports.prototype['cannonReloadingTime'] = undefined;

  /**
   * True if the robot in last command fired a missile.
   * @member {Boolean} firedNewMissile
   */
  exports.prototype['firedNewMissile'] = undefined;

  /**
   * @member {module:model/ScanStatus} scanStatus
   */
  exports.prototype['scanStatus'] = undefined;




  return exports;
}));
