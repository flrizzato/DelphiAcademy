/**
 * Wraps a Google Map in an Ext.Component using the [Google Maps API](http://code.google.com/apis/maps/documentation/v3/introduction.html).
 *
 * This component will automatically include the google maps API script from:
 * `//maps.google.com/maps/api/js`
 *
 * ## Example
 *
 *     Ext.Viewport.add({
 *         xtype: 'map',
 *         useCurrentLocation: true
 *     });
 */
Ext.define('Ext.ux.google.Map', {
    extend: 'Ext.Container',
    xtype : ['map', 'google-map'],
    alternateClassName: 'Ext.Map',
    requires: ['Ext.util.Geolocation'],
    mixins: ['Ext.mixin.Mashup'],

    requires: [
        'Ext.data.StoreManager'
    ],

    requiredScripts: [
        '//maps.googleapis.com/maps/api/js{options}'
    ],

    isMap: true,

    /**
     * @event maprender
     * Fired when Map initially rendered.
     * @param {Ext.ux.google.Map} this
     * @param {google.maps.Map} map The rendered google.map.Map instance
     */

    /**
     * @event centerchange
     * Fired when map is panned around.
     * @param {Ext.ux.google.Map} this
     * @param {google.maps.Map} map The rendered google.map.Map instance
     * @param {google.maps.LatLng} center The current LatLng center of the map
     */

    /**
     * @event typechange
     * Fired when display type of the map changes.
     * @param {Ext.ux.google.Map} this
     * @param {google.maps.Map} map The rendered google.map.Map instance
     * @param {Number} mapType The current display type of the map
     */

    /**
     * @event zoomchange
     * Fired when map is zoomed.
     * @param {Ext.ux.google.Map} this
     * @param {google.maps.Map} map The rendered google.map.Map instance
     * @param {Number} zoomLevel The current zoom level of the map
     */

    /**
     * @event markerclick
     * Fired when the marker icon was clicked.
     * @param {Ext.ux.google.Map} map This map instance
     * @param {Object} info Information about this event
     * @param {Number} info.index The index of the marker record
     * @param {Ext.data.Model} info.record The record associated to the marker
     * @param {google.maps.Marker} info.marker The [Google Map marker](https://developers.google.com/maps/documentation/javascript/3.exp/reference#Marker)
     * @param {google.maps.MouseEvent} info.event The [Google Map event](https://developers.google.com/maps/documentation/javascript/3.exp/reference#MouseEvent)
     */

    /**
     * @event markerdblclick
     * Fired when the marker icon was double clicked.
     * @param {Ext.ux.google.Map} map This map instance
     * @param {Object} info Information about this event
     * @param {Number} info.index The index of the marker record
     * @param {Ext.data.Model} info.record The record associated to the marker
     * @param {google.maps.Marker} info.marker The [Google Map marker](https://developers.google.com/maps/documentation/javascript/3.exp/reference#Marker)
     * @param {google.maps.MouseEvent} info.event The [Google Map event](https://developers.google.com/maps/documentation/javascript/3.exp/reference#MouseEvent)
     */

    /**
     * @event markerdrag
     * Repeatedly fired while the user drags the marker.
     * @param {Ext.ux.google.Map} map This map instance
     * @param {Object} info Information about this event
     * @param {Number} info.index The index of the marker record
     * @param {Ext.data.Model} info.record The record associated to the marker
     * @param {google.maps.Marker} info.marker The [Google Map marker](https://developers.google.com/maps/documentation/javascript/3.exp/reference#Marker)
     * @param {google.maps.MouseEvent} info.event The [Google Map event](https://developers.google.com/maps/documentation/javascript/3.exp/reference#MouseEvent)
     */

    /**
     * @event markerdragend
     * Fired when the user stops dragging the marker.
     * @param {Ext.ux.google.Map} map This map instance
     * @param {Object} info Information about this event
     * @param {Number} info.index The index of the marker record
     * @param {Ext.data.Model} info.record The record associated to the marker
     * @param {google.maps.Marker} info.marker The [Google Map marker](https://developers.google.com/maps/documentation/javascript/3.exp/reference#Marker)
     * @param {google.maps.MouseEvent} info.event The [Google Map event](https://developers.google.com/maps/documentation/javascript/3.exp/reference#MouseEvent)
     */

    /**
     * @event markerdragstart
     * Fired when the user starts dragging the marker.
     * @param {Ext.ux.google.Map} map This map instance
     * @param {Object} info Information about this event
     * @param {Number} info.index The index of the marker record
     * @param {Ext.data.Model} info.record The record associated to the marker
     * @param {google.maps.Marker} info.marker The [Google Map marker](https://developers.google.com/maps/documentation/javascript/3.exp/reference#Marker)
     * @param {google.maps.MouseEvent} info.event The [Google Map event](https://developers.google.com/maps/documentation/javascript/3.exp/reference#MouseEvent)
     */

    /**
     * @event markermousedown
     * Fired for a mousedown on the marker.
     * @param {Ext.ux.google.Map} map This map instance
     * @param {Object} info Information about this event
     * @param {Number} info.index The index of the marker record
     * @param {Ext.data.Model} info.record The record associated to the marker
     * @param {google.maps.Marker} info.marker The [Google Map marker](https://developers.google.com/maps/documentation/javascript/3.exp/reference#Marker)
     * @param {google.maps.MouseEvent} info.event The [Google Map event](https://developers.google.com/maps/documentation/javascript/3.exp/reference#MouseEvent)
     */

    /**
     * @event markermouseout
     * Fired when the mouse leaves the area of the marker icon.
     * @param {Ext.ux.google.Map} map This map instance
     * @param {Object} info Information about this event
     * @param {Number} info.index The index of the marker record
     * @param {Ext.data.Model} info.record The record associated to the marker
     * @param {google.maps.Marker} info.marker The [Google Map marker](https://developers.google.com/maps/documentation/javascript/3.exp/reference#Marker)
     * @param {google.maps.MouseEvent} info.event The [Google Map event](https://developers.google.com/maps/documentation/javascript/3.exp/reference#MouseEvent)
     */

    /**
     * @event markermouseover
     * Fired when the mouse enters the area of the marker icon.
     * @param {Ext.ux.google.Map} map This map instance
     * @param {Object} info Information about this event
     * @param {Number} info.index The index of the marker record
     * @param {Ext.data.Model} info.record The record associated to the marker
     * @param {google.maps.Marker} info.marker The [Google Map marker](https://developers.google.com/maps/documentation/javascript/3.exp/reference#Marker)
     * @param {google.maps.MouseEvent} info.event The [Google Map event](https://developers.google.com/maps/documentation/javascript/3.exp/reference#MouseEvent)
     */

    /**
     * @event markermouseup
     * Fired for a mouseup on the marker.
     * @param {Ext.ux.google.Map} map This map instance
     * @param {Object} info Information about this event
     * @param {Number} info.index The index of the marker record
     * @param {Ext.data.Model} info.record The record associated to the marker
     * @param {google.maps.Marker} info.marker The [Google Map marker](https://developers.google.com/maps/documentation/javascript/3.exp/reference#Marker)
     * @param {google.maps.MouseEvent} info.event The [Google Map event](https://developers.google.com/maps/documentation/javascript/3.exp/reference#MouseEvent)
     */

    /**
     * @event markerrightclick
     * Fired for a rightclick on the marker.
     * @param {Ext.ux.google.Map} map This map instance
     * @param {Object} info Information about this event
     * @param {Number} info.index The index of the marker record
     * @param {Ext.data.Model} info.record The record associated to the marker
     * @param {google.maps.Marker} info.marker The [Google Map marker](https://developers.google.com/maps/documentation/javascript/3.exp/reference#Marker)
     * @param {google.maps.MouseEvent} info.event The [Google Map event](https://developers.google.com/maps/documentation/javascript/3.exp/reference#MouseEvent)
     */

    config: {
        /**
         * @cfg {Boolean/Ext.util.Geolocation} useCurrentLocation
         * Pass in true to center the map based on the geolocation coordinates or pass a
         * {@link Ext.util.Geolocation GeoLocation} config to have more control over your GeoLocation options
         * @accessor
         */
        useCurrentLocation: false,

        /**
         * @cfg {google.maps.Map} map
         * The wrapped map.
         * @accessor
         */
        map: null,

        /**
         * @cfg {Ext.util.Geolocation} geo
         * Geolocation provider for the map.
         * @accessor
         */
        geo: null,

        /**
         * @cfg {Object} mapOptions
         * MapOptions as specified by the Google Documentation:
         * [http://code.google.com/apis/maps/documentation/v3/reference.html](http://code.google.com/apis/maps/documentation/v3/reference.html)
         * @accessor
         */
        mapOptions: {},

        /**
         * @cfg {Object} mapListeners
         * Listeners for any Google Maps events specified by the Google Documentation:
         * [http://code.google.com/apis/maps/documentation/v3/reference.html](http://code.google.com/apis/maps/documentation/v3/reference.html)
         *
         * @accessor
         */
        mapListeners: null,

        /**
         * @cfg {Ext.data.Store/Object/Ext.data.Model[]/Ext.ux.google.map.Marker} markers
         * Can be either a Store instance, a configuration object that will be turned into a
         * store, an array of model or a single model (in which case a store will be created).
         * The Store is used to populate the set of markers that will be rendered in the map.
         * Marker options are read through the {@link #markerTemplate} config.
         */
        markers: null,

        /**
         * @cfg {Object/Ext.util.ObjectTemplate} markerTemplate
         * This is a template used to produce marker options from the {@link #markers} records.
         * See {@link Ext.ux.google.map.Marker} for details.
         */
        markerTemplate: {
            title: '{title}',
            position: '{position}',
            animation: '{animation}',       // google.maps.Animation.DROP
            clickable: '{clickable}',
            draggable: '{draggable}',
            visible: '{visible}'
        }
    },

    baseCls: Ext.baseCSSPrefix + 'map',

    constructor: function(config) {
        this.callParent([config]);

        if (!(window.google || {}).maps) {
            this.setHtml('Google Maps API is required');
        }
    },

    initialize: function() {
        this.callParent();
        this.initMap();

        this.on({
            painted: 'onPainted',
            scope: this
        });
        this.bodyElement.on('touchstart', 'onTouchStart', this);
    },

    initMap: function() {
        var map = this.getMap();
        if(!map) {
            var gm = (window.google || {}).maps;
            if(!gm) return null;

            var element = this.mapContainer,
                mapOptions = this.getMapOptions(),
                event = gm.event,
                me = this;

            //Remove the API Required div
            if (element.dom.firstChild) {
                Ext.fly(element.dom.firstChild).destroy();
            }

            if (Ext.os.is.iPad) {
                Ext.merge({
                    navigationControlOptions: {
                        style: gm.NavigationControlStyle.ZOOM_PAN
                    }
                }, mapOptions);
            }

            mapOptions.mapTypeId = mapOptions.mapTypeId || gm.MapTypeId.ROADMAP;
            mapOptions.center = mapOptions.center || new gm.LatLng(37.381592, -122.135672); // Palo Alto

            if (mapOptions.center && mapOptions.center.latitude && !Ext.isFunction(mapOptions.center.lat)) {
                mapOptions.center = new gm.LatLng(mapOptions.center.latitude, mapOptions.center.longitude);
            }

            mapOptions.zoom = mapOptions.zoom || 12;

            map = new gm.Map(element.dom, mapOptions);
            this.setMap(map);

            event.addListener(map, 'zoom_changed', Ext.bind(me.onZoomChange, me));
            event.addListener(map, 'maptypeid_changed', Ext.bind(me.onTypeChange, me));
            event.addListener(map, 'center_changed', Ext.bind(me.onCenterChange, me));
            event.addListenerOnce(map, 'tilesloaded', Ext.bind(me.onTilesLoaded, me));
            this.addMapListeners();
        }
        return this.getMap();
    },

    // added for backwards compatibility for touch < 2.3
    renderMap: function() {
        this.initMap();
    },

    getElementConfig: function() {
        return {
            reference: 'element',
            className: 'x-container',
            children: [{
                reference: 'bodyElement',
                className: 'x-inner',
                children: [{
                    reference: 'mapContainer',
                    className: Ext.baseCSSPrefix + 'map-container'
                }]
            }]
        };
    },

    onTouchStart: function(e) {
        e.makeUnpreventable();
    },

    updateMap: function(map) {
        var markers = this.getMarkers();
        if (markers) {
            markers.each(function(record) {
                var marker = this.getMarkerForRecord(record);
                if (marker) {
                    marker.setMap(map);
                }
            }, this);
        }
    },

    applyMapOptions: function(options) {
        return Ext.merge({}, this.options, options);
    },

    updateMapOptions: function(newOptions) {
        var gm = (window.google || {}).maps,
            map = this.getMap();

        if (gm && map) {
            map.setOptions(newOptions);
        }
    },

    applyMarkers: function(value) {
        if (!value) {
            return null;
        }

        if (value.isStore) {
            return value;
        }

        if (Ext.isArray(value)) {
            value = { data: value };
        } else if (Ext.isObject(value)) {
            value = { data: [value] };
        }

        return Ext.getStore(value);
    },

    updateMarkers: function(curr, prev) {
        var me = this,
            listeners = {
                add: 'onMarkersAdd',
                remove: 'onMarkersRemove',
                itemchange: 'onMarkerChange',
                scope: this
            };

        if (prev && prev.isStore) {
            prev.getData().un(listeners);
            me.removeMarkers(prev.getRange());
        }

        if (curr && curr.isStore) {
            me.addMarkers(curr.getRange());
            curr.getData().on(listeners);
        }
    },

    applyMarkerTemplate: function (value) {
        return Ext.util.ObjectTemplate.create(value);
    },

    updateMarkerTemplate: function(value) {
        var markers = this.getMarkers();
        if (markers) {
            this.refreshMarkers(markers.getRange());
        }
    },

    doMapCenter: function() {
        this.setMapCenter(this.getMapOptions().center);
    },

    getMapOptions: function() {
        return Ext.merge({}, this.options || this.getInitialConfig('mapOptions'));
    },

    updateUseCurrentLocation: function(useCurrentLocation) {
        this.setGeo(useCurrentLocation);
        if (!useCurrentLocation) {
            this.setMapCenter();
        }
    },

    applyGeo: function(config) {
        return Ext.factory(config, Ext.util.Geolocation, this.getGeo());
    },

    updateGeo: function(newGeo, oldGeo) {
        var events = {
            locationupdate : 'onGeoUpdate',
            locationerror : 'onGeoError',
            scope : this
        };

        if (oldGeo) {
            oldGeo.un(events);
        }

        if (newGeo) {
            newGeo.on(events);
            newGeo.updateLocation();
        }
    },

    /**
     * @private
     */
    onPainted: function() {
        var gm = (window.google || {}).maps,
            map = this.getMap(),
            center;

        if (gm && map) {
            center = map.getCenter();

            gm.event.trigger(map, 'resize');

            if (center) {
                map.setCenter(center);
            }
        }
    },

    /**
     * @private
     */
	onTilesLoaded: function() {
		this.fireEvent('maprender', this, this.getMap());
	},

    /**
     * @private
     */
    addMapListeners: function() {
        var gm = (window.google || {}).maps,
            map = this.getMap(),
            mapListeners = this.getMapListeners();


        if (gm) {
            var event = gm.event,
                me = this,
                listener, scope, fn, callbackFn, handle;
            if (Ext.isSimpleObject(mapListeners)) {
                for (var eventType in mapListeners) {
                    listener = mapListeners[eventType];
                    if (Ext.isSimpleObject(listener)) {
                        scope = listener.scope;
                        fn = listener.fn;
                    } else if (Ext.isFunction(listener)) {
                        scope = null;
                        fn = listener;
                    }

                    if (fn) {
                        callbackFn = function() {
                            this.fn.apply(this.scope, [me]);
                            if(this.handle) {
                                event.removeListener(this.handle);
                                delete this.handle;
                                delete this.fn;
                                delete this.scope;
                            }
                        };
                        handle = event.addListener(map, eventType, Ext.bind(callbackFn, callbackFn));
                        callbackFn.fn = fn;
                        callbackFn.scope = scope;
                        if(listener.single === true) callbackFn.handle = handle;
                    }
                }
            }
        }
    },

    /**
     * @private
     */
    onGeoUpdate: function(geo) {
        if (geo) {
            this.setMapCenter(new google.maps.LatLng(geo.getLatitude(), geo.getLongitude()));
        }
    },

    /**
     * @method
     * @private
     */
    onGeoError: Ext.emptyFn,

    /**
     * Moves the map center to a google.maps.LatLng object representing to the target location,
     * a marker record from the {@link #cfg-markers markers} store, or to the designated
     * coordinates hash of the form:
     *
     *     { latitude: 37.381592, longitude: -122.135672 }
     *
     * @param {Object/Ext.data.Model/google.maps.LatLng} coordinates Object representing the
     * desired latitude and longitude upon which to center the map.
     */
    setMapCenter: function(coordinates) {
        var me = this,
            map = me.getMap(),
            mapOptions = me.getMapOptions(),
            gm = (window.google || {}).maps,
            marker;

        if (gm) {
            if (!coordinates) {
                if (map && map.getCenter) {
                    coordinates = map.getCenter();
                }
                else if (mapOptions.hasOwnProperty('center')) {
                    coordinates = mapOptions.center;
                }
                else {
                    coordinates = new gm.LatLng(37.381592, -122.135672); // Palo Alto
                }
            } else if (coordinates.isModel) {
                var marker = me.getMarkerForRecord(coordinates);
                coordinates = marker && marker.position;
            }

            if (coordinates && !(coordinates instanceof gm.LatLng) && 'longitude' in coordinates) {
                coordinates = new gm.LatLng(coordinates.latitude, coordinates.longitude);
            }

            if (!map) {
                mapOptions.center = mapOptions.center || coordinates;
                me.renderMap();
                map = me.getMap();
            }

            if (map && coordinates instanceof gm.LatLng) {
                map.panTo(coordinates);
            }
            else {
                this.options = Ext.apply(this.getMapOptions(), {
                    center: coordinates
                });
            }
        }
    },

    /**
     * Scales and pans the view to ensure that the given markers fits inside the map view.
     * @param {Ext.data.Model[]} records The markers records to fit in view.
     */
    fitMarkersInView: function(records) {
        var me = this,
            map = me.getMap(),
            b2 = map.getBounds(),
            markers = me.getMarkers(),
            gm = (window.google || {}).maps,
            b1, b1ne, b1sw, b2ne, b2sw;

        if (!map || !b2 || !markers) {
            return;
        }

        if (Ext.isEmpty(records)) {
            records = markers.getRange();
            if (Ext.isEmpty(records)) {
                return;
            }
        }

        b1 = new gm.LatLngBounds();
        Ext.each(records, function(record) {
            var marker = me.getMarkerForRecord(record);
            if (marker) {
                b1.extend(marker.getPosition());
            }
        });

        b1ne = b1.getNorthEast();
        b1sw = b1.getSouthWest();
        b2ne = b2.getNorthEast();
        b2sw = b2.getSouthWest();

        if ((b1ne.lat() - b1sw.lat()) > (b2ne.lat() - b2sw.lat()) ||
            (b1ne.lng() - b1sw.lng()) > (b2ne.lng() - b2sw.lng())) {
            map.fitBounds(b1);
        } else {
            map.panToBounds(b1);
        }
    },

    /**
     * @private
     */
    onZoomChange : function() {
        var mapOptions = this.getMapOptions(),
            map = this.getMap(),
            zoom;

        zoom = (map && map.getZoom) ? map.getZoom() : mapOptions.zoom || 10;

        this.options = Ext.apply(mapOptions, {
            zoom: zoom
        });

        this.fireEvent('zoomchange', this, map, zoom);
    },

    /**
     * @private
     */
    onTypeChange : function() {
        var mapOptions = this.getMapOptions(),
            map = this.getMap(),
            mapTypeId;

        mapTypeId = (map && map.getMapTypeId) ? map.getMapTypeId() : mapOptions.mapTypeId;

        this.options = Ext.apply(mapOptions, {
            mapTypeId: mapTypeId
        });

        this.fireEvent('typechange', this, map, mapTypeId);
    },

    /**
     * @private
     */
    onCenterChange: function() {
        var mapOptions = this.getMapOptions(),
            map = this.getMap(),
            center;

        center = (map && map.getCenter) ? map.getCenter() : mapOptions.center;

        this.options = Ext.apply(mapOptions, {
            center: center
        });

        this.fireEvent('centerchange', this, map, center);

    },

    doDestroy: function() {
        Ext.destroy(this.getGeo());
        var map = this.getMap();

        if (map && (window.google || {}).maps) {
            google.maps.event.clearInstanceListeners(map);
        }

        this.callParent();
    },

    privates: {

        // See google.map.Marker API
        // https://developers.google.com/maps/documentation/javascript/3.exp/reference#Marker

        markerEvents: [
            'click',
            'dblclick',
            'drag',
            'dragend',
            'dragstart',
            'mousedown',
            'mouseout',
            'mouseover',
            'mouseup',
            'rightclick'
        ],

        getMarkerForRecord: function(record) {
            var expando = record && Ext.getExpando(record, this.getId());
            return (expando && expando.marker) || null;
        },

        buildMarkerOptions: function(record, tpl) {
            var options = tpl.apply(record.getData(true)),
                gm = (window.google || {}).maps,
                animation = options.animation;

            if (typeof animation === 'string') {
                options.animation = gm.Animation[animation] || null;
            }

            return options;
        },

        addMarkers: function(records) {
            var me = this,
                eid = me.getId(),
                map = me.getMap(),
                tpl = me.getMarkerTemplate(),
                gm = (window.google || {}).maps,
                store = me.getMarkers(),
                events = me.markerEvents;

            Ext.each(records, function(record) {
                var index = store.indexOf(record),
                    options = me.buildMarkerOptions(record, tpl),
                    marker = new gm.Marker(Ext.apply(options, { map: map })),
                    listeners = events.map(function(type) {
                        return marker.addListener(type, function(event) {
                            me.fireEvent('marker' + type, me, {
                                index: index,
                                record: record,
                                marker: marker,
                                event: event
                            });
                        });
                    });

                Ext.setExpando(record, eid, {
                    listeners: listeners,
                    marker: marker
                });
            });
        },

        removeMarkers: function(records) {
            var eid = this.getId();
            Ext.each(records, function(record) {
                var expando = Ext.getExpando(record, eid),
                    marker = expando && expando.marker;

                if (marker) {
                    marker.setMap(null);
                    Ext.each(expando.listeners || [], function(listener) {
                        listener.remove();
                    });
                }

                Ext.setExpando(record, eid, undefined);
            });
        },

        refreshMarkers: function(records) {
            var me = this,
                tpl = me.getMarkerTemplate(),
                count = records.length,
                record, marker, i;

            for (i = 0; i < count; ++i) {
                record = records[i];
                marker = me.getMarkerForRecord(record);
                if (marker) {
                    marker.setOptions(me.buildMarkerOptions(record, tpl));
                }
            }
        },

        onMarkersAdd: function(collection , details) {
            this.addMarkers(details.items);
        },

        onMarkersRemove: function(collection , details) {
            this.removeMarkers(details.items);
        },

        onMarkerChange: function(collection, details) {
            this.refreshMarkers([details.item]);
        }
    }
});
