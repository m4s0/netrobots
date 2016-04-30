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
    root.NetRobotsApi.DriveCommand = factory(root.NetRobotsApi.ApiClient);
  }
}(this, function(ApiClient) {
  'use strict';

  /**
   * The DriveCommand model module.
   * @module model/DriveCommand
   * @version 2.0
   */

  /**
   * Constructs a new <code>DriveCommand</code>.
   * @alias module:model/DriveCommand
   * @class
   * @param direction
   * @param speed
   */
  var exports = function(direction, speed) {

    this['direction'] = direction;
    this['speed'] = speed;
  };

  /**
   * Constructs a <code>DriveCommand</code> from a plain JavaScript object, optionally creating a new instance.
   * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
   * @param {Object} data The plain JavaScript object bearing properties of interest.
   * @param {module:model/DriveCommand} obj Optional instance to populate.
   * @return {module:model/DriveCommand} The populated <code>DriveCommand</code> instance.
   */
  exports.constructFromObject = function(data, obj) {
    if (data) { 
      obj = obj || new exports();

      if (data.hasOwnProperty('direction')) {
        obj['direction'] = ApiClient.convertToType(data['direction'], 'Number');
      }
      if (data.hasOwnProperty('speed')) {
        obj['speed'] = ApiClient.convertToType(data['speed'], 'Number');
      }
    }
    return obj;
  }


  /**
   * Direction expressed in degrees. 0 degree is EAST, 90 degree is NORTH, 180 degree is WEST, 279 degree is SOUTH
   * @member {Number} direction
   */
  exports.prototype['direction'] = undefined;

  /**
   * @member {Number} speed
   */
  exports.prototype['speed'] = undefined;




  return exports;
}));
