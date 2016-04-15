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
    root.NetRobotsApi.EventExplosion = factory(root.NetRobotsApi.ApiClient, root.NetRobotsApi.Event, root.NetRobotsApi.RobotInfo);
  }
}(this, function(ApiClient, Event, RobotInfo) {
  'use strict';

  /**
   * The EventExplosion model module.
   * @module model/EventExplosion
   * @version 2.0.0
   */

  /**
   * Constructs a new <code>EventExplosion</code>.
   * @alias module:model/EventExplosion
   * @class
   * @extends module:model/Event
   * @param eventType
   * @param activationTime
   * @param robot
   * @param damage
   */
  var exports = function(eventType, activationTime, robot, damage) {
    Event.call(this, eventType, activationTime);
    this['eventType'] = eventType;
    this['robot'] = robot;

    this['damage'] = damage;
  };

  /**
   * Constructs a <code>EventExplosion</code> from a plain JavaScript object, optionally creating a new instance.
   * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
   * @param {Object} data The plain JavaScript object bearing properties of interest.
   * @param {module:model/EventExplosion} obj Optional instance to populate.
   * @return {module:model/EventExplosion} The populated <code>EventExplosion</code> instance.
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
      if (data.hasOwnProperty('hitRobot')) {
        obj['hitRobot'] = RobotInfo.constructFromObject(data['hitRobot']);
      }
      if (data.hasOwnProperty('damage')) {
        obj['damage'] = ApiClient.convertToType(data['damage'], 'Number');
      }
    }
    return obj;
  }

  exports.prototype = Object.create(Event.prototype);
  exports.prototype.constructor = exports;


  /**
   * tag 5
   * @member {Number} eventType
   */
  exports.prototype['eventType'] = undefined;

  /**
   * @member {module:model/RobotInfo} robot
   */
  exports.prototype['robot'] = undefined;

  /**
   * @member {module:model/RobotInfo} hitRobot
   */
  exports.prototype['hitRobot'] = undefined;

  /**
   * @member {Number} damage
   */
  exports.prototype['damage'] = undefined;




  return exports;
}));
