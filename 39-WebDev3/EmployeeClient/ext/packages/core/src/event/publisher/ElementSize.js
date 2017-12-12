/**
 * @private
 */
Ext.define('Ext.event.publisher.ElementSize', {

    extend: 'Ext.event.publisher.Publisher',

    requires: [
        'Ext.util.SizeMonitor'
    ],

    type: 'size',

    handledEvents: ['resize'],

    constructor: function() {
        this.monitors = {};
        this.subscribers = {};

        this.callParent(arguments);
    },

    subscribe: function(element) {
        var id = element.id,
            subscribers = this.subscribers,
            monitors = this.monitors;

        if (subscribers[id]) {
            ++subscribers[id];
        } else {
            subscribers[id] = 1;

            monitors[id] = new Ext.util.SizeMonitor({
                element: element,
                callback: this.onElementResize,
                scope: this,
                args: [element]
            });
        }

        element.on('painted', 'forceRefresh', monitors[id]);

        return true;
    },

    unsubscribe: function(element) {
        var id = element.id,
            subscribers = this.subscribers,
            monitors = this.monitors,
            sizeMonitor;

        if (subscribers[id] && !--subscribers[id]) {
            delete subscribers[id];
            sizeMonitor = monitors[id];
            element.un('painted', 'forceRefresh', sizeMonitor);
            sizeMonitor.destroy();
            delete monitors[id];
        }

        if (element.activeRead) {
            Ext.TaskQueue.cancelRead(element.activeRead);
        }
    },
    
    fireElementResize: function(element, info) {
        delete element.activeRead;
        this.fire(element, 'resize', [element, info]);
    },

    onElementResize: function(element, info) {
        if (!element.activeRead) {
            element.activeRead = Ext.TaskQueue.requestRead(
                'fireElementResize', this, [element, info]
                //<debug>
                , !!element.$skipResourceCheck
                //</debug>
            );
        }
    }

    //<debug>
    // This is useful for unit testing so we can force resizes
    // to take place synchronously when we know they have changed
    ,privates: {
        syncRefresh: function(elements) {
            elements = Ext.Array.from(elements);

            var len = elements.length,
                i = 0,
                el, monitor;

            for (i = 0; i < len; ++i) {
                el = elements[i];
                if (typeof el !== 'string') {
                    el = el.id;
                }
                monitor = this.monitors[el];
                if (monitor) {
                    monitor.forceRefresh();
                }
            }
            // This just pushes onto the RAF queue.
            Ext.TaskQueue.flush();

            // Flush the RAF queue to make this truly synchronous.
            Ext.Function.fireElevatedHandlers();
        }
    }
    //</debug>
}, function(ElementSize) {
    ElementSize.instance = new ElementSize();
});
