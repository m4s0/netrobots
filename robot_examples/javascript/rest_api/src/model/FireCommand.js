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
    root.NetRobotsApi.FireCommand = factory(root.NetRobotsApi.ApiClient);
  }
}(this, function(ApiClient) {
  'use strict';

  /**
   * The FireCommand model module.
   * @module model/FireCommand
   * @version 2.0.0
   */

  /**
   * Constructs a new <code>FireCommand</code>.
   * Missiles have a balistic trajectory, so they do not intercept robots in the middle of the path, but they damage only robot at the end of the path. Missile outside arena make no arm.
   * @alias module:model/FireCommand
   * @class
   * @param direction
   * @param distance
   */
  var exports = function(direction, distance) {

    this['direction'] = direction;
    this['distance'] = distance;
  };

  /**
   * Constructs a <code>FireCommand</code> from a plain JavaScript object, optionally creating a new instance.
   * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
   * @param {Object} data The plain JavaScript object bearing properties of interest.
   * @param {module:model/FireCommand} obj Optional instance to populate.
   * @return {module:model/FireCommand} The populated <code>FireCommand</code> instance.
   */
  exports.constructFromObject = function(data, obj) {
    if (data) { 
      obj = obj || new exports();

      if (data.hasOwnProperty('direction')) {
        obj['direction'] = ApiClient.convertToType(data['direction'], 'Number');
      }
      if (data.hasOwnProperty('distance')) {
        obj['distance'] = ApiClient.convertToType(data['distance'], 'Number');
      }
    }
    return obj;
  }


  /**
   * Direction expressed in degrees. 0 degree is EAST, 90 degree is NORTH, 180 degree is WEST, 270 degree is SOUTH
   * @member {Number} direction
   */
  exports.prototype['direction'] = undefined;

  /**
   * @member {Number} distance
   */
  exports.prototype['distance'] = undefined;




  return exports;
}));
