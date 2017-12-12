Ext.define('Ext.dataview.BoundListLocation', {
    extend: 'Ext.dataview.Location',

    next: function(options) {
        var me = this,
            candidate = me.nextItem(options),
            item = candidate && candidate.get();

        // BoundListLocations do not check focusability - they are never focused
        while (candidate && (!item || !candidate.record)) {
            // Wrapped round. Give up.
            if (candidate.equals(me)) {
                return;
            }
            candidate = candidate.nextItem(options);
            item = candidate && candidate.get();
        }
        return candidate;
    },

    previous: function(options) {
        var me = this,
            candidate = me.previousItem(options),
            item = candidate && candidate.get();

        // BoundListLocations do not check focusability - they are never focused
        while (candidate && (!item || !candidate.record)) {
            // Wrapped round. Give up.
            if (candidate.equals(me)) {
                return;
            }
            candidate = candidate.previousItem(options);
            item = candidate && candidate.get();
        }
        return candidate;
    }
});