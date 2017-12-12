/**
 * Provided for convenience, this model exposes the default Google Map Marker options.
 *
 * See https://developers.google.com/maps/documentation/javascript/3.exp/reference#MarkerOptions
 *
 * ## Fields
 *
 * - position {Object} - The marker position (required)
 * - position.lat {Number} - Latitude in degrees
 * - position.lng {Number} - Longitude in degrees
 * - title {String} - The rollover text (default: null)
 * - animation {String/Number} - The animation to play when the marker is added to the map.
 *    Can be either null (no animation), a string ("BOUNCE" or "DROP"") or a value from
 *   [google.maps.Animation](https://developers.google.com/maps/documentation/javascript/3.exp/reference#Animation)
 * - clickable {Boolean} - Whether the marker receives mouse and touch events (default: true)
 * - draggable {Boolean} - Whether the marker can be dragged (default: false)
 * - draggable {Boolean} - Whether the marker is visible (default: true)
 *
 * ## Custom model
 *
 * It's not required to inherit from this model in order to display markers. By providing the
 * suitable  {@link Ext.ux.google.Map#cfg-markerTemplate markerTemplate}, marker options can
 * be extracted from any records, for example:
 *
 *      Ext.define('MyApp.model.Office', {
 *          extend: 'Ext.data.Model',
 *          fields: [
 *              'name',
 *              'address',
 *              'latitute',
 *              'longitude'
 *          ]
 *      });
 *
 * and the associated view config:
 *
 *      {
 *          xtype: 'map',
 *          store: 'offices',
 *          markerTemplate: {
 *              title: '{name}',
 *              position: {
 *                  lat: '{latitute}',
 *                  lng: '{longitude}'
 *              }
 *          }
 *    }
 *
 */
Ext.define('Ext.ux.google.map.Marker', {
    extend: 'Ext.data.Model',

    fields: [{
        name: 'position',
        type: 'auto'
    }, {
        name: 'title',
        type: 'string',
        defaultValue: null
    },  {
        name: 'animation',
        type: 'number',
        defaultValue: 'DROP',
        persist: false
    }, {
        name: 'clickable',
        type: 'boolean',
        defaultValue: true,
        persist: false
    }, {
        name: 'draggable',
        type: 'boolean',
        defaultValue: false,
        persist: false
    }, {
        name: 'visible',
        type: 'boolean',
        defaultValue: true,
        persist: false
    }]
});
