/**
 * Provides a simple Container for HTML5 Video.
 *
 * ## Notes
 *
 * - There are quite a few issues with the `<video>` tag on Android devices. On Android 2+, the video will
 * appear and play on first attempt, but any attempt afterwards will not work.
 *
 * ## Useful Properties
 *
 * - {@link #url}
 * - {@link #autoPause}
 * - {@link #autoResume}
 *
 * ## Useful Methods
 *
 * - {@link #method-pause}
 * - {@link #method-play}
 * - {@link #toggle}
 *
 * ## Example
 *
 *     var panel = Ext.create('Ext.Panel', {
 *         fullscreen: true,
 *         layout: 'fit',
 *         items: [
 *             {
 *                 xtype    : 'video',
 *                 url      : 'porsche911.mov',
 *                 posterUrl: 'porsche.png'
 *             }
 *         ]
 *     });
 */
Ext.define('Ext.Video', {
    extend: 'Ext.Media',
    xtype: 'video',

    config: {
        /**
         * @cfg {String/Array} url
         * Location of the video to play. This should be in H.264 format and in a .mov file format.
         * @accessor
         */

        /**
         * @cfg {String} posterUrl
         * Location of a poster image to be shown before showing the video.
         * @accessor
         */
        posterUrl: null,

        /**
         * @cfg {Boolean} [showPosterOnPause=true] When paused, the {@link #posterUrl}
         * will be shown. If set to `false`, the poster will not be shown when the video
         * is paused.
         *
         * Showing a poster may save on device resources as the `<video>` element is
         * resource intensive whereas the `<img>` is not as intensive. Not showing a poster
         * may slow down parts of the application including scrolling as the device is
         * repainting the screen.
         *
         * @since 6.5.0
         */
        showPosterOnPause: false
    },

    baseCls: Ext.baseCSSPrefix + 'video',

    template: [{
        /**
         * @property {Ext.dom.Element} ghost
         * @private
         */
        reference: 'ghost',
        classList: [Ext.baseCSSPrefix + 'video-ghost']
    }, {
        tag: 'video',
        reference: 'media',
        classList: [Ext.baseCSSPrefix + 'media']
    }],

    initialize: function() {
        var me = this;

        me.callParent();

        me.media.hide();

        me.ghost.on({
            tap: 'onGhostTap',
            scope: me
        });

        me.media.on({
            pause: 'onPause',
            scope: me
        });

        if (Ext.os.is.Android4 || Ext.os.is.iPad) {
            this.isInlineVideo = true;
        }
    },

    applyUrl: function(url) {
        return [].concat(url);
    },

    updateUrl: function(newUrl) {
        var me = this,
            media = me.media,
            newLn = newUrl.length,
            existingSources = media.query('source'),
            oldLn = existingSources.length,
            i;

        for (i = 0; i < oldLn; i++) {
            Ext.fly(existingSources[i]).destroy();
        }

        for (i = 0; i < newLn; i++) {
            media.appendChild(Ext.Element.create({
                tag: 'source',
                src: newUrl[i]
            }));
        }

        if (me.isPlaying()) {
            me.play();
        }
    },

    onActivate: function() {
        this.media.show();
    },

    onDeactivate: function() {
        this.pause();
        this.media.hide();
        this.ghost.show();
    },

    /**
     * @private
     * Called when the {@link #ghost} element is tapped.
     */
    onGhostTap: function() {
        var me = this,
            media = this.media,
            ghost = this.ghost;

        media.show();
        ghost.hide();
        me.play();
    },

    /**
     * @private
     * native video tag display only, move the media down so we can control the Viewport
     */
    onPause: function(e) {
        this.callParent([e]);

        // If the video is seeking, the browser may pause the video before setting
        // the time to wherever the user clicked on.
        if (!this.isInlineVideo && !e.target.seeking && this.getShowPosterOnPause()) {
            this.media.hide();
            this.ghost.show();
        }
    },

    /**
     * @private
     * native video tag display only, move the media down so we can control the Viewport
     */
    onPlay: function(e) {
        this.callParent([e]);

        this.media.show();
    },

    /**
     * Updates the URL to the poster, even if it is rendered.
     * @param {Object} newUrl
     */
    updatePosterUrl: function(newUrl) {
        var ghost = this.ghost;

        if (ghost) {
            ghost.setStyle('background-image', 'url(' + newUrl + ')');
        }
    }
});
