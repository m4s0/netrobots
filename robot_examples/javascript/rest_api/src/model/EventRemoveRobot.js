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
    root.NetRobotsApi.EventRemoveRobot = factory(root.NetRobotsApi.ApiClient, root.NetRobotsApi.Event, root.NetRobotsApi.RobotInfo);
  }
}(this, function(ApiClient, Event, RobotInfo) {
  'use strict';

  /**
   * The EventRemoveRobot model module.
   * @module model/EventRemoveRobot
   * @version 2.0.0
   */

  /**
   * Constructs a new <code>EventRemoveRobot</code>.
   * @alias module:model/EventRemoveRobot
   * @class
   * @extends module:model/Event
   * @param eventType
   * @param activationTime
   * @param robot
   */
  var exports = function(eventType, activationTime, robot) {
    Event.call(this, eventType, activationTime);
    this['eventType'] = eventType;
    this['robot'] = robot;
  };

  /**
   * Constructs a <code>EventRemoveRobot</code> from a plain JavaScript object, optionally creating a new instance.
   * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
   * @param {Object} data The plain JavaScript object bearing properties of interest.
   * @param {module:model/EventRemoveRobot} obj Optional instance to populate.
   * @return {module:model/EventRemoveRobot} The populated <code>EventRemoveRobot</code> instance.
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
    }
    return obj;
  }

  exports.prototype = Object.create(Event.prototype);
  exports.prototype.constructor = exports;


  /**
   * tag 2
   * @member {Number} eventType
   */
  exports.prototype['eventType'] = undefined;

  /**
   * @member {module:model/RobotInfo} robot
   */
  exports.prototype['robot'] = undefined;




  return exports;
}));
