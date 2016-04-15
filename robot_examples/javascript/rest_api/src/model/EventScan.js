(function(root, factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
    define(['../ApiClient', './Event', './RobotInfo'], factory);
  } else if (typeof module === 'object' && module.exports) {
    // CommonJS-like environments that support module.exports, like Node.
    module.exports = factory(require('../ApiClient'), require('./Event'), require('./RobotInfo'));
  } else {
    // Browser globals (root is window)
    if (!root.NetRobotsApi) {
      root.NetRobotsApi = {};
    }
    root.NetRobotsApi.EventScan = factory(root.NetRobotsApi.ApiClient, root.NetRobotsApi.Event, root.NetRobotsApi.RobotInfo);
  }
}(this, function(ApiClient, Event, RobotInfo) {
  'use strict';

  /**
   * The EventScan model module.
   * @module model/EventScan
   * @version 2.0.0
   */

  /**
   * Constructs a new <code>EventScan</code>.
   * @alias module:model/EventScan
   * @class
   * @extends module:model/Event
   * @param eventType
   * @param activationTime
   * @param direction
   * @param semiaperture
   * @param scanMaxDistance
   * @param robot
   */
  var exports = function(eventType, activationTime, direction, semiaperture, scanMaxDistance, robot) {
    Event.call(this, eventType, activationTime);
    this['eventType'] = eventType;
    this['direction'] = direction;
    this['semiaperture'] = semiaperture;
    this['scanMaxDistance'] = scanMaxDistance;
    this['robot'] = robot;

  };

  /**
   * Constructs a <code>EventScan</code> from a plain JavaScript object, optionally creating a new instance.
   * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
   * @param {Object} data The plain JavaScript object bearing properties of interest.
   * @param {module:model/EventScan} obj Optional instance to populate.
   * @return {module:model/EventScan} The populated <code>EventScan</code> instance.
   */
  exports.constructFromObject = function(data, obj) {
    if (data) { 
      obj = obj || new exports();
      Event.constructFromObject(data, obj);
      if (data.hasOwnProperty('eventType')) {
        obj['eventType'] = ApiClient.convertToType(data['eventType'], 'Number');
      }
      if (data.hasOwnProperty('direction')) {
        obj['direction'] = ApiClient.convertToType(data['direction'], 'Number');
      }
      if (data.hasOwnProperty('semiaperture')) {
        obj['semiaperture'] = ApiClient.convertToType(data['semiaperture'], 'Number');
      }
      if (data.hasOwnProperty('scanMaxDistance')) {
        obj['scanMaxDistance'] = ApiClient.convertToType(data['scanMaxDistance'], 'Number');
      }
      if (data.hasOwnProperty('robot')) {
        obj['robot'] = RobotInfo.constructFromObject(data['robot']);
      }
      if (data.hasOwnProperty('hitRobot')) {
        obj['hitRobot'] = RobotInfo.constructFromObject(data['hitRobot']);
      }
    }
    return obj;
  }

  exports.prototype = Object.create(Event.prototype);
  exports.prototype.constructor = exports;


  /**
   * tag 3
   * @member {Number} eventType
   */
  exports.prototype['eventType'] = undefined;

  /**
   * Direction expressed in degrees. 0 degree is EAST, 90 degree is NORTH, 180 degree is WEST, 270 degree is SOUTH
   * @member {Number} direction
   */
  exports.prototype['direction'] = undefined;

  /**
   * The aperture angle, in degree, divided by 2.
   * @member {Number} semiaperture
   */
  exports.prototype['semiaperture'] = undefined;

  /**
   * The maximum possible distance of the scan.
   * @member {Number} scanMaxDistance
   */
  exports.prototype['scanMaxDistance'] = undefined;

  /**
   * @member {module:model/RobotInfo} robot
   */
  exports.prototype['robot'] = undefined;

  /**
   * @member {module:model/RobotInfo} hitRobot
   */
  exports.prototype['hitRobot'] = undefined;




  return exports;
}));
