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
    root.NetRobotsApi.EventCreateRobot = factory(root.NetRobotsApi.ApiClient, root.NetRobotsApi.Event, root.NetRobotsApi.RobotInfo);
  }
}(this, function(ApiClient, Event, RobotInfo) {
  'use strict';

  /**
   * The EventCreateRobot model module.
   * @module model/EventCreateRobot
   * @version 2.0.0
   */

  /**
   * Constructs a new <code>EventCreateRobot</code>.
   * An event signaling to the UI, the creation of a Robot. eventType is 1.
   * @alias module:model/EventCreateRobot
   * @class
   * @extends module:model/Event
   * @param eventType
   * @param activationTime
   * @param robot
   * @param name
   * @param color
   */
  var exports = function(eventType, activationTime, robot, name, color) {
    Event.call(this, eventType, activationTime);
    this['eventType'] = eventType;
    this['robot'] = robot;
    this['name'] = name;
    this['color'] = color;
  };

  /**
   * Constructs a <code>EventCreateRobot</code> from a plain JavaScript object, optionally creating a new instance.
   * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
   * @param {Object} data The plain JavaScript object bearing properties of interest.
   * @param {module:model/EventCreateRobot} obj Optional instance to populate.
   * @return {module:model/EventCreateRobot} The populated <code>EventCreateRobot</code> instance.
   */
  exports.constructFromObject = function(data, obj) {
    if (data) { 
      obj = obj || new exports();
      Event.constructFromObject(data, obj);
      if (data.hasOwnProperty('eventType')) {
        obj['eventType'] = ApiClient.convertToType(data['eventType'], 'Number');
      }
      if (data.hasOwnProperty('robot')) {
        obj['robot'] = RobotInfo.constructFromObject(data['robot']);
      }
      if (data.hasOwnProperty('name')) {
        obj['name'] = ApiClient.convertToType(data['name'], 'String');
      }
      if (data.hasOwnProperty('color')) {
        obj['color'] = ApiClient.convertToType(data['color'], 'String');
      }
    }
    return obj;
  }

  exports.prototype = Object.create(Event.prototype);
  exports.prototype.constructor = exports;


  /**
   * tag 1
   * @member {Number} eventType
   */
  exports.prototype['eventType'] = undefined;

  /**
   * @member {module:model/RobotInfo} robot
   */
  exports.prototype['robot'] = undefined;

  /**
   * Human readable name for the robot.
   * @member {String} name
   */
  exports.prototype['name'] = undefined;

  /**
   * A color assigned to the robot.
   * @member {String} color
   */
  exports.prototype['color'] = undefined;




  return exports;
}));
