// @tag enterprise
/**
 * The AMF Proxy is an {@link Ext.data.proxy.Ajax Ajax Proxy} that requests
 * binary data from a remote server and parses it into records using an
 * {@link Ext.data.amf.Reader AMF Reader} for use in a
 * {@link Ext.data.Store Store}.
 *
 *     Ext.create('Ext.data.Store', {
 *         model: 'Foo',
 *         proxy: {
 *             type: 'amf',
 *             url: 'some/url'
 *         }
 *     });
 *     
 * For a detailed tutorial on using AMF data see the
 * [AMF Guide](../guides/backend_connectors/amf.html).
 */
Ext.define('Ext.data.amf.Proxy', {
    extend: 'Ext.data.proxy.Ajax',

    alias: 'proxy.amf',

    requires: [
        'Ext.data.amf.Reader'
    ],

    /**
     * @cfg binary
     * @inheritdoc
     */
    binary: true,

    /**
     * @cfg reader
     * @inheritdoc
     */
    reader: 'amf'
});
