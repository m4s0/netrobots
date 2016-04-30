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
    root.NetRobotsApi.ScanCommand = factory(root.NetRobotsApi.ApiClient);
  }
}(this, function(ApiClient) {
  'use strict';

  /**
   * The ScanCommand model module.
   * @module model/ScanCommand
   * @version 2.0
   */

  /**
   * Constructs a new <code>ScanCommand</code>.
   * @alias module:model/ScanCommand
   * @class
   * @param direction
   * @param semiaperture
   */
  var exports = function(direction, semiaperture) {

    this['direction'] = direction;
    this['semiaperture'] = semiaperture;
  };

  /**
   * Constructs a <code>ScanCommand</code> from a plain JavaScript object, optionally creating a new instance.
   * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
   * @param {Object} data The plain JavaScript object bearing properties of interest.
   * @param {module:model/ScanCommand} obj Optional instance to populate.
   * @return {module:model/ScanCommand} The populated <code>ScanCommand</code> instance.
   */
  exports.constructFromObject = function(data, obj) {
    if (data) { 
      obj = obj || new exports();

      if (data.hasOwnProperty('direction')) {
        obj['direction'] = ApiClient.convertToType(data['direction'], 'Number');
      }
      if (data.hasOwnProperty('semiaperture')) {
        obj['semiaperture'] = ApiClient.convertToType(data['semiaperture'], 'Number');
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
   * The aperture angle, in degree, divided by 2.
   * @member {Number} semiaperture
   */
  exports.prototype['semiaperture'] = undefined;




  return exports;
}));
