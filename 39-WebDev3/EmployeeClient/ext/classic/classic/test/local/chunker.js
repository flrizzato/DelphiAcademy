var Test = Test || {};

Test.chunks = [
    ['specs/grid/grid-general.js'],
    [
        'specs/grid/grid-aria.js',
        'specs/grid/grid-celledit.js',
        'specs/grid/grid-columns.js',
        'specs/grid/grid-events.js',
        'specs/grid/grid-general-buffered-no-preserve-scroll.js',
        'specs/grid/grid-general-bufffered-preserve-scroll.js',
        'specs/grid/grid-general-locking-from-no-locking.js',
        'specs/grid/grid-general-locking.js',
        'specs/grid/grid-general-paging-buffered-renderer.js',
        'specs/grid/grid-general-window.js',
        'specs/grid/grid-grouping.js',
        'specs/grid/grid-keys.js'
    ],
    [
        'specs/grid/grid-moving-columns.js',
        'specs/grid/grid-rowedit.js',
        'specs/grid/grid-view.js',
        'specs/grid/grid-widgets.js',
        'specs/grid/Panel.js',
        'specs/grid/NavigationModel.js',
        'specs/grid/column/Action.js',
        'specs/grid/column/Boolean.js',
        'specs/grid/column/Check.js',
        'specs/grid/column/Column.js',
        'specs/grid/column/Date.js',
        'specs/grid/column/Number.js',
        'specs/grid/column/RowNumberer.js',
        'specs/grid/column/Template.js',
        'specs/grid/column/Widget.js'
    ]
];

Test.chunker = function(array, chunkNo, numChunks) {
    // We only want to do this special chunking for REALLY slow browsers
    if (!Test.browser.isIE8m && !Test.browser.isIOS && !Test.browser.isAndroid) {
        return false;
    }
    
    var chunks = Test.chunks,
        urls = array.slice(),
        result = [],
        chunk, found, url, size, i, len, j, jlen, k, klen;
    
    // If we're passed a chunk number that we have a definition for, it's easy
    if (chunks[chunkNo]) {
        chunks = chunks[chunkNo];
        
        URLS:
        for (i = 0, len = urls.length; i < len; i++) {
            url = urls[i];
            
            CHUNK:
            for (j = 0, jlen = chunks.length; j < jlen; j++) {
                if (url.indexOf(chunks[j]) !== -1) {
                    result.push(url);
                    
                    if (result.length === chunks.length) {
                        break URLS;
                    }
                    
                    break CHUNK;
                }
            }
        }
        
        // ¯\_(ツ)_/¯
        if (!result || !result.length) {
            return false;
        }
        
        return result;
    }
    
    // If that's the rest, we need to remove URLs mentioned in special chunks first
    for (i = 0, len = urls.length; i < len; i++) {
        url = urls[i];
        found = false;
        
        CHUNK:
        for (j = 0, jlen = chunks.length; j < jlen; j++) {
            chunk = chunks[j];
            
            for (k = 0, klen = chunk.length; k < klen; k++) {
                if (url.indexOf(chunk[k]) !== -1) {
                    found = true;
                    break CHUNK;
                }
            }
        }
        
        if (!found) {
            result.push(url);
        }
    }
    
    urls = result;
    result = [];
    
    // Then fall back to the default splitting algorithm
    size = Math.ceil(urls.length / numChunks);
    
    while (urls.length) {
        result.push(urls.splice(0, size));
    }
    
    chunk = result[chunkNo];
    
    return chunk && chunk.length ? chunk : false;
}
