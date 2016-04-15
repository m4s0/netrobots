(function(root, factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
    define(['../ApiClient', './Event'], factory);
  } else if (typeof module === 'object' && module.exports) {
    // CommonJS-like environments that support module.exports, like Node.
    module.exports = factory(require('../ApiClient'), require('./Event'));
  } else {
    // Browser globals (root is window)
    if (!root.NetRobotsApi) {
      root.NetRobotsApi = {};
    }
    root.NetRobotsApi.BoardInfo = factory(root.NetRobotsApi.ApiClient, root.NetRobotsApi.Event);
  }
}(this, function(ApiClient, Event) {
  'use strict';

  /**
   * The BoardInfo model module.
   * @module model/BoardInfo
   * @version 2.0.0
   */

  /**
   * Constructs a new <code>BoardInfo</code>.
   * Initial settings of the board.
   * @alias module:model/BoardInfo
   * @class
   */
  var exports = function() {









  };

  /**
   * Constructs a <code>BoardInfo</code> from a plain JavaScript object, optionally creating a new instance.
   * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
   * @param {Object} data The plain JavaScript object bearing properties of interest.
   * @param {module:model/BoardInfo} obj Optional instance to populate.
   * @return {module:model/BoardInfo} The populated <code>BoardInfo</code> instance.
   */
  exports.constructFromObject = function(data, obj) {
    if (data) { 
      obj = obj || new exports();

      if (data.hasOwnProperty('maxBoardX')) {
        obj['maxBoardX'] = ApiClient.convertToType(data['maxBoardX'], 'Number');
      }
      if (data.hasOwnProperty('maxBoardY')) {
        obj['maxBoardY'] = ApiClient.convertToType(data['maxBoardY'], 'Number');
      }
      if (data.hasOwnProperty('streamDelay')) {
        obj['streamDelay'] = ApiClient.convertToType(data['streamDelay'], 'Number');
      }
      if (data.hasOwnProperty('turnDeltaTime')) {
        obj['turnDeltaTime'] = ApiClient.convertToType(data['turnDeltaTime'], 'Number');
      }
      if (data.hasOwnProperty('networkLatency')) {
        obj['networkLatency'] = ApiClient.convertToType(data['networkLatency'], 'Number');
      }
      if (data.hasOwnProperty('startTime')) {
        obj['startTime'] = ApiClient.convertToType(data['startTime'], 'Number');
      }
      if (data.hasOwnProperty('endTime')) {
        obj['endTime'] = ApiClient.convertToType(data['endTime'], 'Number');
      }
      if (data.hasOwnProperty('events')) {
        obj['events'] = ApiClient.convertToType(data['events'], [Event]);
      }
    }
    return obj;
  }


  /**
   * @member {Number} maxBoardX
   */
  exports.prototype['maxBoardX'] = undefined;

  /**
   * @member {Number} maxBoardY
   */
  exports.prototype['maxBoardY'] = undefined;

  /**
   * the time in real seconds, beetwen event lists
   * @member {Number} streamDelay
   */
  exports.prototype['streamDelay'] = undefined;

  /**
   * Every robot can send and execute a command only after this simulated interval of time.
   * @member {Number} turnDeltaTime
   */
  exports.prototype['turnDeltaTime'] = undefined;

  /**
   * Every robot can send a command only after this interval of time.
   * @member {Number} networkLatency
   */
  exports.prototype['networkLatency'] = undefined;

  /**
   * The events starts at the specified simulation time.
   * @member {Number} startTime
   */
  exports.prototype['startTime'] = undefined;

  /**
   * The events ends at the specified simulation time.
   * @member {Number} endTime
   */
  exports.prototype['endTime'] = undefined;

  /**
   * @member {Array.<module:model/Event>} events
   */
  exports.prototype['events'] = undefined;




  return exports;
}));
