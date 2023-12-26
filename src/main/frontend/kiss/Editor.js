/**
 * Provide a word-processor-like control.
 * <br><br>
 * This class is a wrapper around the public CKEditor 5 utility.  It is intended that this class be used exclusively
 * rather than any of the raw CKEditor API.  It provides a higher-level and more convenient API.
 * <br><br>
 * Please refer to CKEditor documentation on <a href="https://ckeditor.com/docs">https://ckeditor.com/docs</a> for more information.
 */
class Editor {

    constructor(parentId) {
        this.parentId = parentId;
        this.editorId = parentId + "-editor";
        this.toolbarId = parentId + "-toolbar";
    }

    async initialize() {
        const self = this;

        const parentDiv = document.getElementById(this.parentId);
        parentDiv.style.border = '1px solid black';
        parentDiv.style.backgroundColor = 'white';

        // Create toolbar div
        const toolbarDiv = document.createElement('div');
        toolbarDiv  .id = this.toolbarId;
        parentDiv.appendChild(toolbarDiv  );

        // Create editor div
        const editorDiv = document.createElement('div');
        editorDiv.id = this.editorId;
        const parentHeight = parseInt(parentDiv.style.height);
        editorDiv.style.height = (parentHeight - 41) + 'px';
        parentDiv.appendChild(editorDiv);

        return new Promise(function (resolve, reject) {
            DecoupledEditor
                .create( editorDiv, {
                    fontSize: {
                        options: [
                            9,
                            11,
                            13,
                            'default',
                            17,
                            19,
                            21,
                            23,
                            25
                        ],
                        supportAllValues: true
                    }
                } )
                .then( editor => {
                    toolbarDiv.appendChild(editor.ui.view.toolbar.element);
                    resolve( editor );
                } )
                .catch( error => {
                    console.error( error );
                    reject( undefined );
                } );
        });
    }

    /**
     * Creates a new instance of the Editor class and initializes it.
     *
     * @param {string} parentId - The ID of the editor div.
     * @return {Promise<Editor>} A promise that resolves to the initialized Editor instance.
     */
    static async create(parentId) {
        const ins = new Editor(parentId);
        ins.editor = await ins.initialize();
        Editor.addEditor(ins);
        return ins;
    }

    /**
     * Retrieves the HTML content from the editor.
     *
     * @return {string} The HTML content of the editor.
     */
    getHtml() {
        return this.editor.getData();
    }

    /**
     * Sets the HTML content in the editor.
     *
     * @param html
     */
    setHtml(html) {
        this.editor.setData(html);
    }

    /**
     * Clears the content of the editor.
     */
    clear() {
        this.editor.setData('');
    }

    /**
     * Sets the editor to read-only mode.
     */
    readOnly() {
        this.editor.enableReadOnlyMode('xx');
    }

    /**
     * Sets the editor to read-write mode.
     */
    readWrite() {
        this.editor.disableReadOnlyMode('xx');
    }

    /**
     * Dispose of the editor when it is no longer needed.
     * Generally, Kiss handles this automatically.
     */
    dispose() {
        this.editor.destroy().then(() => {
            const parentDiv = document.getElementById(this.parentId);

            // Check if the first child exists and remove it
            const toolbar = document.getElementById(this.toolbarId);
            if (toolbar) 
                parentDiv.removeChild(toolbar);

            // Check if the second child exists and remove it
            const editor = document.getElementById(this.editorId);
            if (editor) 
                parentDiv.removeChild(editor);
        } ).catch(error => {
            console.log(error);
        });
    }
    
    /**
     * Create a new editor context.
     */
    static newEditorContext() {
        Editor.editorContext.push([]);
    }

    /**
     * Add an editor to the current context.
     *
     * @param editor
     */
    static addEditor(editor) {
        const cc = Editor.editorContext[Editor.editorContext.length - 1];
        cc.push(editor);
    }

    /**
     * Destroy all editors in last context and remove the context
     */
    static popEditorContext() {
        const c = Editor.editorContext.pop();
        if (c)
            for (let i = 0; i < c.length; i++)
                c[i].dispose();
    }

    /**
     * Destroys all popup and screen editors that have been created
     */
    static popAllEditorContexts() {
        while (Editor.editorContext.length)
            Editor.popEditorContext();
    }

    /**
     * Print the contents of the editor
     */
    printEditorContent() {
        const printWindow = window.open('', '_blank');
        printWindow.document.write('<html lang="en"><head><title>Print</title></head><body>');
        printWindow.document.write(this.editor.getData()); // Get the data from CKEditor
        printWindow.document.write('</body></html>');
        printWindow.document.close();

        printWindow.focus();
        printWindow.print();

        printWindow.close();
    }
}

Editor.editorContext = [];      //  An array of arrays.  The outer array represents a stack of contexts.
                                //  The inner array is an array of editors that'll need to be disposed.
                                //  Basically, each context (except the first) represents a popup.
                                //  The first represents the current screen.
                                //  Each inner array contains an array of editors in that context.