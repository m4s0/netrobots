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
    root.NetRobotsApi.EventMissile = factory(root.NetRobotsApi.ApiClient, root.NetRobotsApi.Event, root.NetRobotsApi.RobotInfo);
  }
}(this, function(ApiClient, Event, RobotInfo) {
  'use strict';

  /**
   * The EventMissile model module.
   * @module model/EventMissile
   * @version 2.0.0
   */

  /**
   * Constructs a new <code>EventMissile</code>.
   * @alias module:model/EventMissile
   * @class
   * @extends module:model/Event
   * @param eventType
   * @param activationTime
   * @param robot
   * @param direction
   * @param distance
   * @param speed
   */
  var exports = function(eventType, activationTime, robot, direction, distance, speed) {
    Event.call(this, eventType, activationTime);
    this['eventType'] = eventType;
    this['robot'] = robot;
    this['direction'] = direction;
    this['distance'] = distance;
    this['speed'] = speed;
  };

  /**
   * Constructs a <code>EventMissile</code> from a plain JavaScript object, optionally creating a new instance.
   * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
   * @param {Object} data The plain JavaScript object bearing properties of interest.
   * @param {module:model/EventMissile} obj Optional instance to populate.
   * @return {module:model/EventMissile} The populated <code>EventMissile</code> instance.
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
      if (data.hasOwnProperty('direction')) {
        obj['direction'] = ApiClient.convertToType(data['direction'], 'Number');
      }
      if (data.hasOwnProperty('distance')) {
        obj['distance'] = ApiClient.convertToType(data['distance'], 'Number');
      }
      if (data.hasOwnProperty('speed')) {
        obj['speed'] = ApiClient.convertToType(data['speed'], 'Number');
      }
    }
    return obj;
  }

  exports.prototype = Object.create(Event.prototype);
  exports.prototype.constructor = exports;


  /**
   * tag 4
   * @member {Number} eventType
   */
  exports.prototype['eventType'] = undefined;

  /**
   * @member {module:model/RobotInfo} robot
   */
  exports.prototype['robot'] = undefined;

  /**
   * @member {Number} direction
   */
  exports.prototype['direction'] = undefined;

  /**
   * @member {Number} distance
   */
  exports.prototype['distance'] = undefined;

  /**
   * @member {Number} speed
   */
  exports.prototype['speed'] = undefined;




  return exports;
}));
