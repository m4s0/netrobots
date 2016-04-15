(function(root, factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
    define(['../ApiClient'], factory);
  } else if (typeof module === 'object' && module.exports) {
    // CommonJS-like environments that support module.exports, like Node.
    module.exports = factory(require('../ApiClient'));
  } else {
    // Browser globals (root is window)
    if (!root.NetRobotsApi) {
      root.NetRobotsApi = {};
    }
    root.NetRobotsApi.Event = factory(root.NetRobotsApi.ApiClient);
  }
}(this, function(ApiClient) {
  'use strict';

  /**
   * The Event model module.
   * @module model/Event
   * @version 2.0.0
   */

  /**
   * Constructs a new <code>Event</code>.
   * Base class from which all different type of Events are derived.
   * @alias module:model/Event
   * @class
   * @param eventType
   * @param activationTime
   */
  var exports = function(eventType, activationTime) {

    this['eventType'] = eventType;
    this['activationTime'] = activationTime;
  };

  /**
   * Constructs a <code>Event</code> from a plain JavaScript object, optionally creating a new instance.
   * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
   * @param {Object} data The plain JavaScript object bearing properties of interest.
   * @param {module:model/Event} obj Optional instance to populate.
   * @return {module:model/Event} The populated <code>Event</code> instance.
   */
  exports.constructFromObject = function(data, obj) {
    if (data) { 
      obj = obj || new exports();

      if (data.hasOwnProperty('eventType')) {
        obj['eventType'] = ApiClient.convertToType(data['eventType'], 'Number');
      }
      if (data.hasOwnProperty('activationTime')) {
        obj['activationTime'] = ApiClient.convertToType(data['activationTime'], 'Number');
      }
    }
    return obj;
  }


  /**
   * The type of event generated. Used for simulating object-oriented subclassing.
   * @member {Number} eventType
   */
  exports.prototype['eventType'] = undefined;

  /**
   * When the event become active.
   * @member {Number} activationTime
   */
  exports.prototype['activationTime'] = undefined;




  return exports;
}));
