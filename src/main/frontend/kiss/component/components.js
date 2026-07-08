
/* This file is only used for documentation purposes.  */


/**
 * This class doesn't actually exist.  It is merely used to group and document the HTML components that come with the <em>Kiss</em> system.
 * Each method represents a new HTML tag offered by the <em>Kiss</em> system.  Although these methods are shown with an underscore in their name,
 * the actual tag they represent would have a dash instead.
 * <br><br>
 * In addition to the additional capabilities offered by some of these controls, many are enhancements over features already provided by HTML.
 * These enhancements offer significant additional functionality as well as a more consistent interface.
 * <br><br>
 * In addition to the tag attributes and content documented here, most of these new HTML tags also support the standard attributes
 * that HTML normally supports (such as "style" and "id").  Each of these controls (except <code>radio-button</code>) <em>require</em> the "id" attribute in order
 * for your code to reference the control.
 * <br><br>
 * Components are accessed via the <code>$$</code> global function.  For example, if you have HTML containing:
 * <br><br>
 *     &nbsp;&nbsp;&nbsp;&nbsp; <code>&lt;text-input id="fname"&gt;&lt;/text-input&gt;</code>
 *  <br><br>
 *  You would access that control in your code with <code>$$('fname')</code>  Methods available withing that control are
 *  accessed from that point.  For example, one method available to <code>text-input</code> is <code>hide()</code>.
 *  This method can be accessed as follows:
 *  <br><br>
 *   &nbsp;&nbsp;&nbsp;&nbsp;   <code>$$('fname').hide()</code>
 *  <br><br>
 *  The documentation for each element includes three sections labeled <em>Attributes</em>, <em>Content</em>, and <em>API</em>.
 *  These are used as follows:
 *  <br><br>
 *  &nbsp;&nbsp;&nbsp;&nbsp; <code>&lt;text-input Attributes&gt;Content&lt;/text-input&gt;</code>
 *  <br><br>
 *  The <em>API</em> is what is used within JavaScript.
 *  <br><br>
 *  Functions that don't have a meaningful result otherwise, return 'this' so that function chaining can occur.
 *  <br><br>
 *  All the controls support all the standard HTML global attributes.  The following lists some of the attributes commonly used:
 *  <br><br>
 *  <table>
 *    <tr><th align="left" style="padding-right: 120px;">Common Attributes</th></tr>
 *    <tr><td>    autofocus     </td></tr>
 *    <tr><td>    class="class-name"     </td></tr>
 *    <tr><td>    disabled     </td></tr>
 *    <tr><td>    hidden     </td></tr>
 *    <tr><td>    id="control-id"     </td></tr>
 *    <tr><td>    readonly     </td></tr>
 *    <tr><td>    style="..."     </td></tr>
 *  </table>
 *
 */
class components {

    /**
     * There is no constructor.
     */
    constructor() {}

    /**
     * This HTML tag, "check-box", adds functionality and a consistent and convenient API to the HTML provided <code>checkbox</code> tag.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 100px;">Attribute</th><th align="left">Description</th></tr>
     *     <tr><td>    checked     </td><td>     pre-selects the element               </td></tr>
     * </table>
     * <br>
     * <strong>Content</strong>
     * <br><br>
     *     The <em>Content</em> represents the label associated with the checkbox.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 120px;">API</th><th align="left">Description</th></tr>
     *     <tr><td>    clear()     </td><td>     uncheck the box               </td></tr>
     *     <tr><td>    disable([flg])     </td><td>     the control remains visible but inactive (or the reverse if the optional argument is <code>false</code>)   </td></tr>
     *     <tr><td>    enable([flg])     </td><td>     the control is set to visible and enabled (or the reverse if the optional argument is <code>false</code>)              </td></tr>
     *     <tr><td>    focus()     </td><td>     sets focus on control            </td></tr>
     *     <tr><td>    getValue()     </td><td>     returns <code>true</code> if checked and <code>false</code> if unchecked               </td></tr>
     *     <tr><td>    hide([flg])     </td><td>     the control is hidden (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     *     <tr><td>    isDirty()     </td><td>    <code>true</code> if the user changed its state     </td></tr>
     *     <tr><td>    isDisabled()     </td><td>    <code>true</code> if the control is disabled     </td></tr>
     *     <tr><td>    isHidden()     </td><td>    <code>true</code> if the control is hidden (not visible)    </td></tr>
     *     <tr><td>    isReadOnly()     </td><td>    <code>true</code> if the control is read-only     </td></tr>
     *     <tr><td>    isVisible()     </td><td>    <code>true</code> if the control is visible (not hidden)     </td></tr>
     *     <tr><td>    onChange(fun)     </td><td>     execute <code>fun</code> whenever the state of this control changes. <code>fun</code> is passed the value of the control.  </td></tr>
     *     <tr><td>    processChanges(flg)     </td><td> if <code>false</code>, checkbox should not register changes (no data was changed) See Utils.someControlValueChanged() </td></tr>
     *     <tr><td>    readOnly([flg])     </td><td>    sets control to read-only (or the reverse if the optional argument is <code>false</code>)         </td></tr>
     *     <tr><td>    readWrite([flg])     </td><td>    sets control to read-write (or the reverse if the optional argument is <code>false</code>)        </td></tr>
     *     <tr><td>    setValue(val)     </td><td>     if <code>val</code> is <code>true</code> check the box, uncheck if <code>false</code>               </td></tr>
     *     <tr><td>    show([flg])     </td><td>     the control is made visible (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     * </table>
     */
    static check_box() {}

    /**
     * This HTML tag, "combo-box", is the equivalent of the Microsoft Windows combobox control.  It is a drop-down list
     * that <em>also</em> allows the user to type in a value that is not in the list (free entry).  Like a Windows combobox,
     * the drop-down arrow always reopens the complete list regardless of what has been typed (unlike the native HTML
     * <code>datalist</code>, which filters itself away once the typed text matches no option).
     * <br><br>
     * Each list item has a <em>label</em> (the text shown, typed, and selected by the user) and an optional underlying
     * <em>value</em> and arbitrary associated <em>data</em>.  When the user types text that is not in the list,
     * <code>getValue()</code> returns that typed text as-is.  When the user selects (or types) a known list item,
     * <code>getValue()</code> returns that item's underlying value.  When <code>add</code> is called with only one argument,
     * the value and the label are the same (a plain string list).
     * <br><br>
     * Unlike <code>drop-down</code>, <code>clear()</code> here empties the entered/selected text (the typical expectation
     * for a control the user types into); use <code>clearList()</code> to remove the suggestion list.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 100px;">Attribute</th><th align="left">Description</th></tr>
     *     <tr><td>    placeholder="text"     </td><td>     text shown in the control when it is empty (standard HTML attribute, passed through)               </td></tr>
     *     <tr><td>    required     </td><td>     an entry (selected or typed) is required               </td></tr>
     * </table>
     * <br>
     * <strong>Content</strong>
     * <br><br>
     *     The <em>Content</em> represents the HTML that would normally be inside an HTML <code>datalist</code> element — i.e.
     *     a static list of <code>&lt;option value="..."&gt;</code> suggestions.  This would only be used in cases of a static
     *     list.  List contents that depend on data would use the <code>add</code> / <code>addItems</code> / <code>fill</code> methods.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 120px;">API</th><th align="left">Description</th></tr>
     *     <tr><td>    add(val [, lbl, data])     </td><td>     add a new list item.  <code>val</code> is the underlying value, <code>lbl</code> is the text shown in the list (defaults to <code>val</code> if omitted), and <code>data</code> represents optional and arbitrary data associated to the item               </td></tr>
     *     <tr><td>    addItems(items, valField, lblField [, dataField]) </td><td>  used to add an array of items at one time. <code>items</code> is the array of items to add.  <code>valField</code> and <code>lblField</code> are the names of the fields in the array.  <code>lblField</code> can also be a function that formats the label.  It is passed the row in <code>items</code>. <code>dataField</code> is the name of a field whose data is stored along with the item.  If null, the whole item is stored. </td></tr>
     *     <tr><td>    clear()     </td><td>     clear the entered/selected text (the suggestion list is left intact)               </td></tr>
     *     <tr><td>    clearList()     </td><td>     remove all items from the suggestion list               </td></tr>
     *     <tr><td>    disable([flg])     </td><td>     the control remains visible but inactive (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     *     <tr><td>    enable([flg])     </td><td>     the control is set to visible and enabled (or the reverse if the optional argument is <code>false</code>)              </td></tr>
     *     <tr><td>    fill(selectedItem, items, valField, labelField [, dataField]) </td><td>   clear the list and fill it, then select/show the item whose value is <code>selectedItem</code> (or clear the entry if none) </td></tr>
     *     <tr><td>    focus()     </td><td>    sets focus on the control        </td></tr>
     *     <tr><td>    getAllData()     </td><td>     returns the map of all label-to-data associations             </td></tr>
     *     <tr><td>    getAllLabels()     </td><td>     returns an array of all the suggestion labels             </td></tr>
     *     <tr><td>    getData()     </td><td>     returns the data associated to the currently shown item (or <code>undefined</code> for a free entry)              </td></tr>
     *     <tr><td>    getLabel()     </td><td>     same as <code>getText()</code>             </td></tr>
     *     <tr><td>    getText()     </td><td>     returns the text currently shown in the control (the label), whether a list item or free entry             </td></tr>
     *     <tr><td>    getValue()     </td><td>     returns the underlying value of the shown item, or the typed text itself when it is a free entry not in the list   </td></tr>
     *     <tr><td>    hide([flg])     </td><td>     the control is hidden (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     *     <tr><td>    isDirty()     </td><td>     <code>true</code> if the user changed the value      </td></tr>
     *     <tr><td>    isDisabled()     </td><td>     <code>true</code> if control is disabled      </td></tr>
     *     <tr><td>    isError(desc)     </td><td>     used for error checking. If error, display error message and return <code>true</code>.  <code>desc</code> is a description of the user field.               </td></tr>
     *     <tr><td>    isHidden()     </td><td>     <code>true</code> if control is hidden (not visible)      </td></tr>
     *     <tr><td>    isReadOnly()     </td><td>     <code>true</code> if control is read-only     </td></tr>
     *     <tr><td>    isVisible()     </td><td>     <code>true</code> if control is visible (not hidden)      </td></tr>
     *     <tr><td>    onCChange(fun)     </td><td> execute <code>fun</code> immediately when the value is changed by the user, <code>fun</code> is passed the control value    </td></tr>
     *     <tr><td>    onChange(fun)     </td><td>     execute <code>fun</code> whenever the user commits a change (selection or leaving the field after editing). <code>fun</code> is called as <code>fun(value, text, data)</code>              </td></tr>
     *     <tr><td>    onEnter(fun)      </td><td>  execute <code>fun</code> when the enter key is hit   </td></tr>
     *     <tr><td>    readOnly([flg])     </td><td>    make control read-only (or the reverse if the optional argument is <code>false</code>)      </td></tr>
     *     <tr><td>    readWrite([flg])     </td><td>    make control read-write (or the reverse if the optional argument is <code>false</code>)      </td></tr>
     *     <tr><td>    setText(val)     </td><td>     set the text shown in the control directly (treats <code>val</code> as the displayed label)             </td></tr>
     *     <tr><td>    setValue(val)     </td><td>     show the list item whose underlying value is <code>val</code>; if none matches, <code>val</code> is shown as free text             </td></tr>
     *     <tr><td>    show([flg])     </td><td>     the control is made visible (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     *     <tr><td>    size()     </td><td>    returns the number of items in the suggestion list              </td></tr>
     *     <tr><td>    triggerGlobalChange(flg)     </td><td>    Default <code>true</code>.  If <code>false</code> then control changes will not trigger a global control change.  See <code>Utils.someControlValueChanged()</code>   </td></tr>
     * </table>
     */
    static combo_box() {}

    /**
     * This HTML tag, "date-input", adds functionality and a consistent and convenient API for user date input.
     * This control is custom and doesn't use the native browser date input offering the advantage of a significantly smaller width requirements.  Also see native-date-input tag.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 100px;">Attribute</th><th align="left">Description</th></tr>
     *     <tr><td>    max="20181231"     </td><td>     the maximum date allowed  (also accepts "2018-12-31", "2/22/25", etc.)             </td></tr>
     *     <tr><td>    min="20180101"     </td><td>     the minimum date allowed  (also accepts "2018-12-31", "2/22/25", etc.)             </td></tr>
     *     <tr><td>    no-placeholder     </td><td>     do not display mm/dd/yyyy when field is empty  </td></tr>
     *     <tr><td>    required     </td><td>     an entry is required               </td></tr>
     * </table>
     * <br>
     * <strong>Content</strong>
     * <br><br>
     *     The <em>Content</em> represents the placeholder or what is shown as a prompt inside the control when there is no value.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 120px;">API</th><th align="left">Description</th></tr>
     *     <tr><td>    clear()     </td><td>     clear the control value               </td></tr>
     *     <tr><td>    disable([flg])     </td><td>     the control remains visible but inactive (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     *     <tr><td>    enable([flg])     </td><td>     the control is set to visible and enabled (or the reverse if the optional argument is <code>false</code>)              </td></tr>
     *     <tr><td>    focus()     </td><td>     sets the focus (where the cursor is located) to this control              </td></tr>
     *     <tr><td>    getDateValue()     </td><td>     returns the date as a <code>Date</code> instance             </td></tr>
     *     <tr><td>    getIntValue()     </td><td>     returns the date as an integer with the "YYYYMMDD" format             </td></tr>
     *     <tr><td>    getSQLValue()     </td><td>     returns the date as a string with the "YYYY-MM-DD" format             </td></tr>
     *     <tr><td>    hide([flg])     </td><td>     the control is hidden (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     *     <tr><td>    isDirty()     </td><td>    <code>true</code> if the user changed the value       </td></tr>
     *     <tr><td>    isDisabled()     </td><td>    <code>true</code> if the control is disabled       </td></tr>
     *     <tr><td>    isError(desc)     </td><td>     used for error checking. If error, display error message and return <code>true</code>.  <code>desc</code> is a description of the user field.               </td></tr>
     *     <tr><td>    isHidden()     </td><td>   <code>true</code> if the control is hidden (not visible)    </td></tr>
     *     <tr><td>    isReadOnly()     </td><td>   <code>true</code> if the control is read-only    </td></tr>
     *     <tr><td>    isVisible()     </td><td>   <code>true</code> if the control is visible (not hidden)    </td></tr>
     *     <tr><td>    onCChange(fun)     </td><td> execute <code>fun</code> immediately when the value is changed by the user, <code>fun</code> is passed the control value    </td></tr>
     *     <tr><td>    onChange(fun)     </td><td>     execute <code>fun</code>whenever the user exits the control if the value changed, <code>fun</code> is passed the control value               </td></tr>
     *     <tr><td>    onEnter(fun)      </td><td>  execute fun when enter key hit   </td></tr>
     *     <tr><td>    readOnly([flg])     </td><td>    set control to read-only (or the reverse if the optional argument is <code>false</code>)  </td></tr>
     *     <tr><td>    readWrite([flg])     </td><td>    set control to read-write (or the reverse if the optional argument is <code>false</code>)     </td></tr>
     *     <tr><td>    setMaxValue(val)     </td><td>     sets the maximum value the control will accept  <code>val</code> may be a <code>Date</code>, <code>number</code> (20180608), or <code>string</code> ("2018-06-08")              </td></tr>
     *     <tr><td>    setMinValue(val)     </td><td>     sets the minimum value the control will accept  <code>val</code> may be a <code>Date</code>, <code>number</code> (20180608), or <code>string</code> ("2018-06-08")              </td></tr>
     *     <tr><td>    setValue(val)     </td><td>     sets the control value.  <code>val</code> may be a <code>Date</code>, <code>number</code> (20180608), or <code>string</code> ("2018-06-08")              </td></tr>
     *     <tr><td>    show([flg])     </td><td>     the control is made visible (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     * </table>
     */
    static date_input() {}

    /**
     * This HTML tag, "drop-down", adds functionality and a consistent and convenient API to the HTML provided <code>select</code> tag.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 100px;">Attribute</th><th align="left">Description</th></tr>
     *     <tr><td>    default-option="label"     </td><td>     what is shown before the user makes a selection.  This would often be something like "(choose)"               </td></tr>
     *     <tr><td>    required     </td><td>     a selection is required               </td></tr>
     * </table>
     * <br>
     * <strong>Content</strong>
     * <br><br>
     *     The <em>Content</em> represents the HTML that would normally be inside an HTML <code>select</code> element.  This would only be used
     *     in cases of a static list.  List contents that depended on data would use the <code>add</code> method.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 120px;">API</th><th align="left">Description</th></tr>
     *     <tr><td>    add(val, lbl, data)     </td><td>     add a new list item.  <code>val</code> is the value associated to the option, <code>lbl</code> is the text shown in the list, and <code>data</data> represents optional and arbitrary data associated to the option               </td></tr>
     *     <tr><td>    addItems(items, valField, lblField [, dataField]) </td><td>  used to add an array of items at one time. <code>items</code> is the array of items to add.  <code>valField</code> and <code>lblField</code> are the names of the fields in the array.  <code>lblField</code> an also be a function that formats the label.  It is passed the row in <code>items</code>. <code>dataField</code> is the name of a field whose data is stored along with the item.  If null, the whole item is stored. </td></tr>
     *     <tr><td>    clear()     </td><td>     remove the list contents except the <code>default-option</code>               </td></tr>
     *     <tr><td>    disable([flg])     </td><td>     the control remains visible but inactive (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     *     <tr><td>    enable([flg])     </td><td>     the control is set to visible and enabled (or the reverse if the optional argument is <code>false</code>)              </td></tr>
     *     <tr><td>    fill(selectedItem, items, valField, labelField [, dataField]) </td><td>   clear and fill a dropdown selecting the specified item </td></tr>
     *     <tr><td>    focus()     </td><td>    sets focus on the control        </td></tr>
     *     <tr><td>    getAllLabels()     </td><td>     returns an array of all the labels             </td></tr>
     *     <tr><td>    getAllData()     </td><td>     returns an array of all the data associated with the control             </td></tr>
     *     <tr><td>    getData(idx)     </td><td>     returns the data associated to an option.  If <code>idx</code> is undefined, the selected row is used otherwise the row indexed by <code>idx</code> is used.              </td></tr>
     *     <tr><td>    getLabel(idx)     </td><td>     returns the label associated to an option. If <code>idx</code> is undefined, the selected row is used otherwise the row indexed by <code>idx</code> is used.             </td></tr>
     *     <tr><td>    getValue(idx)     </td><td>     returns the string value associated to an option (returns an array if <code>multiple</code> attribute included).  If <code>idx</code> is undefined, the selected row is used otherwise the row indexed by <code>idx</code> is used.   </td></tr>
     *     <tr><td>    hide([flg])     </td><td>     the control is hidden (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     *     <tr><td>    isDirty([flg])     </td><td>     <code>true</code> if user changed value (or the reverse if the optional argument is <code>false</code>)      </td></tr>
     *     <tr><td>    isDisabled()     </td><td>     <code>true</code> if control is disabled      </td></tr>
     *     <tr><td>    isHidden()     </td><td>     <code>true</code> if control is hidden (not visible)      </td></tr>
     *     <tr><td>    isError(desc)     </td><td>     used for error checking. If error, display error message and return <code>true</code>.  <code>desc</code> is a description of the user field.               </td></tr>
     *     <tr><td>    isReadOnly()     </td><td>     <code>true</code> if control is read-only     </td></tr>
     *     <tr><td>    isVisible()     </td><td>     <code>true</code> if control is visible (not hidden)      </td></tr>
     *     <tr><td>    onChange(fun)     </td><td>     execute <code>fun</code>whenever the state of this control changes. <code>fun</code> is called as follows <code>fun(val, lbl, data)</code>              </td></tr>
     *     <tr><td>    readOnly([flg])     </td><td>    make control read-only (or the reverse if the optional argument is <code>false</code>)      </td></tr>
     *     <tr><td>    readWrite([flg])     </td><td>    make control read-write (or the reverse if the optional argument is <code>false</code>)      </td></tr>
     *     <tr><td>    removeByIndex(idx)     </td><td>     remove the row indicated by <code>idx</code>             </td></tr>
     *     <tr><td>    selectedIndex()     </td><td>     returns the index of the selected item  (-1 if none)             </td></tr>
     *     <tr><td>    selectIndex(row)     </td><td>     selects the indicated row index            </td></tr>
     *     <tr><td>    setLabel(lbl, idx)     </td><td>     sets a row label to <code>lbl</code>.  If <code>idx</code> is undefined, the selected row is affected otherwise the row indexed by <code>idx</code> is updated.             </td></tr>
     *     <tr><td>    setValue(val, idx)     </td><td>     sets a row value to <code>val</code>.  If <code>idx</code> is undefined, the selected row is affected otherwise the row indexed by <code>idx</code> is updated.  </td></tr>
     *     <tr><td>    show([flg])     </td><td>     the control is made visible (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     *     <tr><td>    size()     </td><td>    returns the number of rows in the list (including <code>default-option</code>              </td></tr>
     *     <tr><td>    triggerGlobalChange(flg)     </td><td>    Default <code>true</code>.  If <code>false</code> then control changes will not trigger a global control change.  See <code>Utils.someControlValueChanged()</code>   </td></tr>
     * </table>
     */
    static drop_down() {}

    /**
     * This HTML tag, "duration-input", provides a control where the user can enter a duration as two integers separated by a colon.
     * The first number is the number of hours and the second is the number of minutes (0-59).  If the user does not enter a colon,
     * the value is treated as hours only with an implied <code>:00</code> for the minutes (so <code>3</code> means 3 hours, 0 minutes).
     * The control allows only digits and a single colon.  This represents a duration, not a time of day.
     * <br><br>
     * The "value" of this control is the total number of minutes (an integer).  <code>getValue()</code> returns
     * <code>0</code> when the field is blank or contains an invalid value.
     * <br><br>
     * If the <code>decimal-hours</code> attribute is set, the user enters and the control displays a decimal number of hours
     * (e.g. <code>1.25</code> means 1 hour 15 minutes) instead of <code>H:MM</code>.  Only digits and at most one decimal point
     * are accepted.  The JavaScript API is unchanged: <code>getValue()</code> still returns the total number of minutes
     * (so <code>1.25</code> entered returns <code>75</code>) and <code>setValue(75)</code> displays <code>1.25</code>.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 100px;">Attribute</th><th align="left">Description</th></tr>
     *     <tr><td>    decimal-hours     </td><td>     accept and display the value as decimal hours (e.g. 1.25) instead of H:MM             </td></tr>
     *     <tr><td>    min="1:30"     </td><td>     the minimum duration allowed (also accepts a total-minutes integer or, in decimal-hours mode, a decimal-hours value such as <code>"1.5"</code>)              </td></tr>
     *     <tr><td>    max="8:00"     </td><td>     the maximum duration allowed (also accepts a total-minutes integer or, in decimal-hours mode, a decimal-hours value such as <code>"8.0"</code>)               </td></tr>
     *     <tr><td>    no-placeholder     </td><td>     do not display the format hint when field is empty  </td></tr>
     *     <tr><td>    required     </td><td>     an entry is required               </td></tr>
     *     <tr><td>    size="20"     </td><td>     width of control in number of characters (default 20)              </td></tr>
     * </table>
     * <br>
     * <strong>Content</strong>
     * <br><br>
     *     The <em>Content</em> represents the placeholder or what is shown as a prompt inside the control when there is no value.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 120px;">API</th><th align="left">Description</th></tr>
     *     <tr><td>    clear()     </td><td>     remove the value associated with the control               </td></tr>
     *     <tr><td>    disable([flg])     </td><td>     the control remains visible but inactive (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     *     <tr><td>    enable([flg])     </td><td>     the control is set to visible and enabled (or the reverse if the optional argument is <code>false</code>)              </td></tr>
     *     <tr><td>    focus()     </td><td>     sets the focus (where the cursor is located) to this control              </td></tr>
     *     <tr><td>    getHours()     </td><td>     returns the duration as a floating point number of hours (e.g. 2:15 returns 2.25)             </td></tr>
     *     <tr><td>    getValue()     </td><td>     returns the total number of minutes; <code>0</code> if blank or invalid             </td></tr>
     *     <tr><td>    hide([flg])     </td><td>     the control is hidden (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     *     <tr><td>    isDirty()     </td><td>     <code>true</code> if user changed control value     </td></tr>
     *     <tr><td>    isDisabled()     </td><td>     <code>true</code> if control is disabled    </td></tr>
     *     <tr><td>    isError(desc)     </td><td>     used for error checking. If error, display error message and return <code>true</code>.  <code>desc</code> is a description of the user field.               </td></tr>
     *     <tr><td>    isHidden()     </td><td>     <code>true</code> if control is hidden (not visible)     </td></tr>
     *     <tr><td>    isReadOnly()     </td><td>     <code>true</code> if control is read-only     </td></tr>
     *     <tr><td>    isVisible()     </td><td>     <code>true</code> if control is visible (not hidden)    </td></tr>
     *     <tr><td>    onCChange(fun)     </td><td>     execute <code>fun</code> immediately when the value is changed by the user, <code>fun</code> is passed the control value    </td></tr>
     *     <tr><td>    onChange(fun)     </td><td>     execute <code>fun</code> whenever the user exits the control if the value changed, <code>fun</code> is passed the control value               </td></tr>
     *     <tr><td>    onEnter(fun)      </td><td>  execute fun when enter key hit   </td></tr>
     *     <tr><td>    readOnly([flg])     </td><td>   sets control to read-only (or the reverse if the optional argument is <code>false</code>)   </td></tr>
     *     <tr><td>    readWrite([flg])     </td><td>    sets control to read-write (or the reverse if the optional argument is <code>false</code>)      </td></tr>
     *     <tr><td>    setHours(val)     </td><td>     sets the control value from a floating point number of hours (e.g. 2.25 displays as 2:15)             </td></tr>
     *     <tr><td>    setMaxValue(val)     </td><td>     sets the maximum value the control will accept (accepts "h:mm" string or a total-minutes integer)             </td></tr>
     *     <tr><td>    setMinValue(val)     </td><td>     sets the minimum value the control will accept (accepts "h:mm" string or a total-minutes integer)             </td></tr>
     *     <tr><td>    setValue(val)     </td><td>     sets the control value from a total number of minutes              </td></tr>
     *     <tr><td>    show([flg])     </td><td>     the control is made visible (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     * </table>
     */
    static duration_input() {}

    /**
     * This HTML tag, "file-upload", adds functionality and a consistent and convenient facility uploading files and taking pictures with the camera.
     * <br>
     * Please note: When this control is used, you must use <code>Server.fileUploadSend()</code> rather than <code>Server.call()</code>.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 100px;">Attribute</th><th align="left">Description</th></tr>
     *     <tr><td>    multiple     </td><td>     the user may select multiple files              </td></tr>
     *     <tr><td>    required     </td><td>     an entry is required (at least 1 file)          </td></tr>
     *     <tr><td>    accept     </td><td>     types of files allowed (see HTML input type="file")          </td></tr>
     *     <tr><td>    custom     </td><td>     rather than the native HTML control, use a Kiss button to upload files.         </td></tr>
     * </table>
     * <br>
     * <strong>Content</strong>
     * <br><br>
     *     The <em>Content</em> of this control is unused.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 120px;">API</th><th align="left">Description</th></tr>
     *     <tr><td>    clear()     </td><td>     erases the contents of the control               </td></tr>
     *     <tr><td>    click() </td><td> simulate a user click on the control </td></tr>
     *     <tr><td>    disable([flg])     </td><td>     the control remains visible but inactive (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     *     <tr><td>    enable([flg])     </td><td>     the control is set to visible and enabled (or the reverse if the optional argument is <code>false</code>)              </td></tr>
     *     <tr><td>    focus()     </td><td>     sets the focus (where the cursor is located) to this control              </td></tr>
     *     <tr><td>    getFormData() </td><td>  gets the upload file data for transmission to the back-end  </td></tr>
     *     <tr><td>    hide([flg])     </td><td>     the control is hidden (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     *     <tr><td>    isDirty()     </td><td>  has the control contents been changed by user      </td></tr>
     *     <tr><td>    isError(desc)     </td><td>     used for error checking. If error, display error message and return <code>true</code>.  <code>desc</code> is a description of the user field.               </td></tr>
     *     <tr><td>    isDisabled()     </td><td> is the control disabled? </td></tr>
     *     <tr><td>    isHidden()     </td><td> is the control hidden? </td></tr>
     *     <tr><td>    isReadOnly()     </td><td> is the control read-only?    </td></tr>
     *     <tr><td>    isVisible()     </td><td> is the control visible?   </td></tr>
     *     <tr><td>    numberOfUploadFiles() </td><td> the number of files the user selected is returned </td></tr>
     *     <tr><td>    onChange(fun)     </td><td>  execute fun when control changes              </td></tr>
     *     <tr><td>    readOnly([flg])     </td><td> set control to read-only (or the reverse if the optional argument is <code>false</code>)    </td></tr>
     *     <tr><td>    readWrite([flg])     </td><td> set control to read-write (or the reverse if the optional argument is <code>false</code>)    </td></tr>
     *     <tr><td>    show([flg])     </td><td>     the control is made visible (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     *     <tr><td>    uploadFile(idx) </td><td> the JavaScript <code>FileList</code> object of file number <code>idx</code>. If <code>idx</code> is missing, the whole FileList is returned. </td></tr>
     *     <tr><td>    uploadFileExtension(idx) </td><td> the file name extension of file number <code>idx</code> </td></tr>
     *     <tr><td>    uploadFilename(idx) </td><td> the name of file number <code>idx</code> </td></tr>
     * </table>
     */
    static file_upload() {}

    /**
     * This HTML tag, "list-box", adds functionality and a consistent and convenient API to the HTML provided <code>select</code> tag.
     * <br><br>
     * Be sure to call the <code>clear()</code> method between uses otherwise the system won't detect the same file
     * being accessed.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 100px;">Attribute</th><th align="left">Description</th></tr>
     *     <tr><td>    default-option="label"     </td><td>     what is the default selection     </td></tr>
     *     <tr><td>    multiple     </td><td>     multiple entries may be selected (an array will be returned)               </td></tr>
     *     <tr><td>    required     </td><td>     a selection is required               </td></tr>
     *     <tr><td>    size="20"     </td><td>    the <em>minimum</em> number of visible lines (will expand to fill the area it is in) </td></tr>
     * </table>
     * <br>
     * <strong>Content</strong>
     * <br><br>
     *     The <em>Content</em> represents the HTML that would normally be inside an HTML <code>select</code> element.  This would only be used
     *     in cases of a static list.  List contents that depended on data would use the <code>add</code> method.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 120px;">API</th><th align="left">Description</th></tr>
     *     <tr><td>    add(val, lbl, data)     </td><td>     add a new list item.  <code>val</code> is the value associated to the option, <code>lbl</code> is the text shown in the list, and <code>data</data> represents optional and arbitrary data associated to the option               </td></tr>
     *     <tr><td>    addItems(items, valField, lblField [, dataField]) </td><td>  used to add an array of items at one time. <code>items</code> is the array of items to add.  <code>valField</code> and <code>lblField</code> are the names of the fields in the array.  <code>lblField</code> an also be a function that formats the label.  It is passed the row in <code>items</code>. <code>dataField</code> is the name of a field whose data is stored along with the item.  If null, the whole item is stored. </td></tr>
     *     <tr><td>    clear()     </td><td>     remove the list contents except the <code>default-option</code>               </td></tr>
     *     <tr><td>    clearSelection()     </td><td>     de-select all elements              </td></tr>
     *     <tr><td>    disable([flg])     </td><td>     the control remains visible but inactive (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     *     <tr><td>    enable([flg])     </td><td>     the control is set to visible and enabled (or the reverse if the optional argument is <code>false</code>)              </td></tr>
     *     <tr><td>    focus()     </td><td>    sets focus on control            </td></tr>
     *     <tr><td>    getAllLabels()     </td><td>     returns an array of all the labels             </td></tr>
     *     <tr><td>    getAllData()     </td><td>     returns an array of all the data associated with the control             </td></tr>
     *     <tr><td>    getData(idx)     </td><td>     returns the data associated to an option.  If <code>idx</code> is undefined, the selected row is used otherwise the row indexed by <code>idx</code> is used.              </td></tr>
     *     <tr><td>    getLabel(idx)     </td><td>     returns the label associated to an option. If <code>idx</code> is undefined, the selected row is used otherwise the row indexed by <code>idx</code> is used.             </td></tr>
     *     <tr><td>    getValue(idx)     </td><td>     If <code>idx</code> is undefined and the <code>multiple</code> attribute is set, returns an array of all selected values.  If <code>idx</code> is undefined and <code>multiple</code> is not set, returns the single selected value.  If <code>idx</code> is provided, returns the value at that row index.   </td></tr>
     *     <tr><td>    hide([flg])     </td><td>     the control is hidden (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     *     <tr><td>    isDirty()     </td><td>    <code>true</code> if user has changed control value     </td></tr>
     *     <tr><td>    isDisabled()     </td><td>    <code>true</code> if control is disabled     </td></tr>
     *     <tr><td>    isError(desc)     </td><td>     used for error checking. If error, display error message and return <code>true</code>.  <code>desc</code> is a description of the user field.               </td></tr>
     *     <tr><td>    isHidden()     </td><td>    <code>true</code> if control is hidden (not visible)     </td></tr>
     *     <tr><td>    isReadOnly()     </td><td>     <code>true</code> if control is read-only     </td></tr>
     *     <tr><td>    isVisible()     </td><td>    <code>true</code> if control is visible (not hidden)     </td></tr>
     *     <tr><td>    onChange(fun)     </td><td>     execute <code>fun</code>whenever the state of this control changes. <code>fun</code> is called as follows <code>fun(val, lbl, data)</code>              </td></tr>
     *     <tr><td>    onClick(fun)     </td><td>     execute <code>fun</code>whenever the user clicks on an item. <code>fun</code> is called as follows <code>fun(val, lbl, data)</code>              </td></tr>
     *     <tr><td>    onDblClick(fun)     </td><td>     execute <code>fun</code>whenever the user double-clicks on an item. <code>fun</code> is called as follows <code>fun(val, lbl, data) (Note that double-click does not function on mobile devices.)</code>              </td></tr>
     *     <tr><td>    readOnly([flg])     </td><td>     sets control to read-only (or the reverse if the optional argument is <code>false</code>)    </td></tr>
     *     <tr><td>    readWrite([flg])     </td><td>     sets control to read-write (or the reverse if the optional argument is <code>false</code>)    </td></tr>
     *     <tr><td>    removeByIndex(idx)     </td><td>     remove the row indicated by <code>idx</code>             </td></tr>
     *     <tr><td>    setLabel(lbl, idx)     </td><td>     sets a row label to <code>lbl</code>.  If <code>idx</code> is undefined, the selected row is affected otherwise the row indexed by <code>idx</code> is updated.             </td></tr>
     *     <tr><td>    setValue(val, idx)     </td><td>     sets a row value to <code>val</code>.  If <code>idx</code> is undefined, the selected row is affected otherwise the row indexed by <code>idx</code> is updated.  </td></tr>
     *     <tr><td>    selectedIndex()     </td><td>     returns the index of the selected item  (-1 if none)             </td></tr>
     *     <tr><td>    selectIndex(row)     </td><td>     selects the indicated row index            </td></tr>
     *     <tr><td>    show([flg])     </td><td>     the control is made visible (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     *     <tr><td>    size()     </td><td>    returns the number of rows in the list               </td></tr>
     *     <tr><td>    triggerGlobalChange(flg)     </td><td>    Default <code>true</code>.  If <code>false</code> then control changes will not trigger a global control change.  See <code>Utils.someControlValueChanged()</code>   </td></tr>
     * </table>
     */
    static list_box() {}

    /**
     * This HTML tag, "native-date-input", adds functionality and a consistent and convenient API to the HTML provided date input.
     * Also see the date-input tag.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 100px;">Attribute</th><th align="left">Description</th></tr>
     *     <tr><td>    max="20181231"     </td><td>     the maximum date allowed (also accepts "2018-12-31", "2/22/25", etc.)              </td></tr>
     *     <tr><td>    min="20180101"     </td><td>     the minimum date allowed  (also accepts "2018-12-31", "2/22/25", etc.)             </td></tr>
     *     <tr><td>    required     </td><td>     an entry is required               </td></tr>
     * </table>
     * <br>
     * <strong>Content</strong>
     * <br><br>
     *     The <em>Content</em> represents the placeholder or what is shown as a prompt inside the control when there is no value.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 120px;">API</th><th align="left">Description</th></tr>
     *     <tr><td>    clear()     </td><td>     clear the control value               </td></tr>
     *     <tr><td>    disable([flg])     </td><td>     the control remains visible but inactive (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     *     <tr><td>    enable([flg])     </td><td>     the control is set to visible and enabled (or the reverse if the optional argument is <code>false</code>)              </td></tr>
     *     <tr><td>    focus()     </td><td>     sets the focus (where the cursor is located) to this control              </td></tr>
     *     <tr><td>    getDateValue()     </td><td>     returns the date as a <code>Date</code> instance             </td></tr>
     *     <tr><td>    getIntValue()     </td><td>     returns the date as an integer with the "YYYYMMDD" format             </td></tr>
     *     <tr><td>    getSQLValue()     </td><td>     returns the date as a string with the "YYYY-MM-DD" format             </td></tr>
     *     <tr><td>    hide([flg])     </td><td>     the control is hidden (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     *     <tr><td>    isDirty()     </td><td>    <code>true</code> if the user changed the value       </td></tr>
     *     <tr><td>    isDisabled()     </td><td>    <code>true</code> if the control is disabled       </td></tr>
     *     <tr><td>    isError(desc)     </td><td>     used for error checking. If error, display error message and return <code>true</code>.  <code>desc</code> is a description of the user field.               </td></tr>
     *     <tr><td>    isHidden()     </td><td>   <code>true</code> if the control is hidden (not visible)    </td></tr>
     *     <tr><td>    isReadOnly()     </td><td>   <code>true</code> if the control is read-only    </td></tr>
     *     <tr><td>    isVisible()     </td><td>   <code>true</code> if the control is visible (not hidden)    </td></tr>
     *     <tr><td>    onCChange(fun)     </td><td> execute <code>fun</code> immediately when the value is changed by the user, <code>fun</code> is passed the control value    </td></tr>
     *     <tr><td>    onChange(fun)     </td><td>     execute <code>fun</code>whenever the user exits the control if the value changed, <code>fun</code> is passed the control value               </td></tr>
     *     <tr><td>    onEnter(fun)      </td><td>  execute fun when enter key hit   </td></tr>
     *     <tr><td>    readOnly([flg])     </td><td>    set control to read-only (or the reverse if the optional argument is <code>false</code>)  </td></tr>
     *     <tr><td>    readWrite([flg])     </td><td>    set control to read-write (or the reverse if the optional argument is <code>false</code>)     </td></tr>
     *     <tr><td>    setMaxValue(val)     </td><td>     sets the maximum value the control will accept  <code>val</code> may be a <code>Date</code>, <code>number</code> (20180608), or <code>string</code> ("2018-06-08")              </td></tr>
     *     <tr><td>    setMinValue(val)     </td><td>     sets the minimum value the control will accept.  <code>val</code> may be a <code>Date</code>, <code>number</code> (20180608), or <code>string</code> ("2018-06-08")              </td></tr>
     *     <tr><td>    setValue(val)     </td><td>     sets the control value.  <code>val</code> may be a <code>Date</code>, <code>number</code> (20180608), or <code>string</code> ("2018-06-08")              </td></tr>
     *     <tr><td>    show([flg])     </td><td>     the control is made visible (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     * </table>
     */
    static native_date_input() {}

    /**
     * This HTML tag, "numeric-input", adds functionality and a consistent and convenient API to the HTML input text element. For example, it will
     * only accept numbers, can verify decimal places, and formats the number when the control is exited by the user.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 100px;">Attribute</th><th align="left">Description</th></tr>
     *     <tr><td>    decimal-places="2"     </td><td>     controls the maximum number of digits past the decimal point (default 0) </td></tr>
     *     <tr><td>    dollar-sign     </td><td>     adds a dollar sign when formatting the number               </td></tr>
     *     <tr><td>    left-justify     </td><td>     left-justifies the number (default is right-justified)               </td></tr>
     *     <tr><td>    min="20"     </td><td>     sets the minimum acceptable value (default 0)   </td></tr>
     *     <tr><td>    max="200"     </td><td>     sets the maximum acceptable value               </td></tr>
     *     <tr><td>    money     </td><td>     sets <code>min="0" dollar-sign decimal-places="2"</code>               </td></tr>
     *     <tr><td>    no-comma     </td><td>     do not format number with commas  </td></tr>
     *     <tr><td>    required     </td><td>     an entry is required               </td></tr>
     *     <tr><td>    show-zero     </td><td>    show zero values (instead of blank if zero)               </td></tr>
     *     <tr><td>    size="20"     </td><td>     width of control in number of characters (default 20)              </td></tr>
     * </table>
     * <br>
     * <strong>Content</strong>
     * <br><br>
     *     The <em>Content</em> represents the placeholder or what is shown as a prompt inside the control when there is no value.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 120px;">API</th><th align="left">Description</th></tr>
     *     <tr><td>    clear()     </td><td>     erases the contents of the control               </td></tr>
     *     <tr><td>    disable([flg])     </td><td>     the control remains visible but inactive (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     *     <tr><td>    enable([flg])     </td><td>     the control is set to visible and enabled (or the reverse if the optional argument is <code>false</code>)              </td></tr>
     *     <tr><td>    focus()     </td><td>     sets the focus (where the cursor is located) to this control              </td></tr>
     *     <tr><td>    getValue()     </td><td>     returns the numeric value of the control             </td></tr>
     *     <tr><td>    hide([flg])     </td><td>     the control is hidden (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     *     <tr><td>    isDirty()     </td><td>     did user change control content?   </td></tr>
     *     <tr><td>    isDisabled()     </td><td>     is control disabled?   </td></tr>
     *     <tr><td>    isError(desc)     </td><td>     used for error checking. If error, display error message and return <code>true</code>.  <code>desc</code> is a description of the user field.               </td></tr>
     *     <tr><td>    isHidden()     </td><td>     is control hidden?   </td></tr>
     *     <tr><td>    isReadOnly()     </td><td>     is control read-only?    </td></tr>
     *     <tr><td>    isVisible()     </td><td>     is control visible?   </td></tr>
     *     <tr><td>    onCChange(fun)     </td><td> execute <code>fun</code> immediately when the value is changed by the user, <code>fun</code> is passed the control value    </td></tr>
     *     <tr><td>    onChange(fun)     </td><td>     execute <code>fun</code>whenever the user exits the control if the value changed, <code>fun</code> is passed the control value               </td></tr>
     *     <tr><td>    onEnter(fun)      </td><td>  execute fun when enter key hit   </td></tr>
     *     <tr><td>    readOnly([flg])     </td><td> set control to read-only (or the reverse if the optional argument is <code>false</code>)    </td></tr>
     *     <tr><td>    readWrite([flg])     </td><td> set control to read-write (or the reverse if the optional argument is <code>false</code>)    </td></tr>
     *     <tr><td>    setMaxValue(val)     </td><td>     sets the maximum value the control will accept              </td></tr>
     *     <tr><td>    setMinValue(val)     </td><td>     sets the minimum value the control will accept             </td></tr>
     *     <tr><td>    setValue(val)     </td><td>     sets the numeric value of the control              </td></tr>
     *     <tr><td>    show([flg])     </td><td>     the control is made visible (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     * </table>
     */
    static numeric_input() {}

    /**
     * This HTML tag, "picture", adds the ability to display an image.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 120px;">API</th><th align="left">Description</th></tr>
     *     <tr><td>    clear()     </td><td>     erases the image               </td></tr>
     *     <tr><td>    hide([flg])     </td><td>     the image is hidden (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     *     <tr><td>    isHidden()     </td><td>     is image hidden?   </td></tr>
     *     <tr><td>    isVisible()     </td><td>     is image visible?   </td></tr>
     *     <tr><td>    onclick(fun)     </td><td>     <code>fun</code> is executed when the user clicks on the image               </td></tr>
     *     <tr><td>    setValue(filename, image)     </td><td>     sets the image to be displayed           </td></tr>
     *     <tr><td>    show([flg])     </td><td>     the image is made visible (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     * </table>
     */
    static picture() {}

    /**
     * This HTML tag, "popup", adds the ability to define a popup window.  Within it, the tags "popup-title" and "popup-body"
     * should be used to define the respective parts of the popup window.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 100px;">Attribute</th><th align="left">Description</th></tr>
     *     <tr><td>    height="400px"     </td><td>     sets the height of the body of the popup window   </td></tr>
     *     <tr><td>    width="200px"     </td><td>     sets the width of the body of the popup window     </td></tr>
     * </table>
     */
    static popup() {}

    /**
     * This HTML tag, "menu-button", creates a button that opens a keyboard-accessible dropdown menu.
     * <br><br>
     * Menu content is declared with child <code>menu-item</code>, <code>menu-separator</code>, and optional
     * <code>menu-trigger</code> tags.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 100px;">Attribute</th><th align="left">Description</th></tr>
     *     <tr><td>    label="Menu"     </td><td>     trigger text when no <code>menu-trigger</code> is supplied     </td></tr>
     *     <tr><td>    align="start"     </td><td>     menu alignment: start or end     </td></tr>
     *     <tr><td>    placement="bottom"     </td><td>     menu placement: bottom or top     </td></tr>
     *     <tr><td>    disabled     </td><td>     disables the trigger     </td></tr>
     *     <tr><td>    close-on-select="false"     </td><td>     leaves the menu open after selection     </td></tr>
     * </table>
     * <table>
     *     <tr><th align="left" style="padding-right: 120px;">API</th><th align="left">Description</th></tr>
     *     <tr><td>    open() / close() / toggle()     </td><td>     controls menu visibility     </td></tr>
     *     <tr><td>    isOpen()     </td><td>     returns whether the menu is visible     </td></tr>
     *     <tr><td>    onselect(fun)     </td><td>     receives <code>(value, label, itemElement)</code> when a menu item is chosen     </td></tr>
     *     <tr><td>    getValue() / setValue(val)     </td><td>     gets or stores the current selected value     </td></tr>
     *     <tr><td>    disable([flg])     </td><td>     disables the trigger (or the reverse if the optional argument is <code>false</code>)     </td></tr>
     *     <tr><td>    enable([flg])     </td><td>     enables the trigger (or the reverse if the optional argument is <code>false</code>)     </td></tr>
     *     <tr><td>    focus()     </td><td>     sets focus on the trigger     </td></tr>
     * </table>
     */
    static menu_button() {}

    /**
     * This HTML tag, "panel-card", creates a generic card or panel surface.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 100px;">Attribute</th><th align="left">Description</th></tr>
     *     <tr><td>    as="section"     </td><td>     rendered element: div, section, article, aside, main, header, or footer     </td></tr>
     *     <tr><td>    no-pad     </td><td>     removes default padding     </td></tr>
     *     <tr><td>    hover     </td><td>     adds a hover elevation state     </td></tr>
     *     <tr><td>    interactive     </td><td>     adds button-like focus and click affordances     </td></tr>
     * </table>
     * <table>
     *     <tr><th align="left" style="padding-right: 120px;">API</th><th align="left">Description</th></tr>
     *     <tr><td>    onclick(fun)     </td><td>     registers a click handler (used with the <code>interactive</code> attribute)     </td></tr>
     *     <tr><td>    hide([flg])     </td><td>     the control is hidden (or the reverse if the optional argument is <code>false</code>)     </td></tr>
     *     <tr><td>    show([flg])     </td><td>     the control is shown (or the reverse if the optional argument is <code>false</code>)     </td></tr>
     *     <tr><td>    isHidden()     </td><td>    <code>true</code> if the control is hidden     </td></tr>
     *     <tr><td>    isVisible()     </td><td>    <code>true</code> if the control is visible     </td></tr>
     * </table>
     */
    static panel_card() {}

    /**
     * This HTML tag, "section-title", creates a reusable section heading row with optional supporting text and actions.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 100px;">Attribute</th><th align="left">Description</th></tr>
     *     <tr><td>    title="Title"     </td><td>     heading text     </td></tr>
     *     <tr><td>    subtitle="Description"     </td><td>     secondary text under the heading     </td></tr>
     *     <tr><td>    eyebrow="Label"     </td><td>     small label above the heading     </td></tr>
     *     <tr><td>    level="2"     </td><td>     heading level from 1 to 6     </td></tr>
     * </table>
     * Child content is rendered as the action area when title attributes are supplied.
     * <table>
     *     <tr><th align="left" style="padding-right: 120px;">API</th><th align="left">Description</th></tr>
     *     <tr><td>    setTitle(text)     </td><td>     updates the heading text     </td></tr>
     *     <tr><td>    setSubtitle(text)     </td><td>     updates the secondary text under the heading     </td></tr>
     * </table>
     */
    static section_title() {}

    /**
     * This HTML tag, "segmented-control", creates a segmented tab/button selector.
     * <br><br>
     * Options are declared with child <code>segment value="...">Label</code> tags.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 100px;">Attribute</th><th align="left">Description</th></tr>
     *     <tr><td>    value="id"     </td><td>     selected segment value     </td></tr>
     *     <tr><td>    equal     </td><td>     segments share equal width     </td></tr>
     *     <tr><td>    disabled     </td><td>     disables all segments     </td></tr>
     * </table>
     * <table>
     *     <tr><th align="left" style="padding-right: 120px;">API</th><th align="left">Description</th></tr>
     *     <tr><td>    getValue() / setValue(val [, silent])     </td><td>     reads or selects a segment     </td></tr>
     *     <tr><td>    onchange(fun)     </td><td>     receives <code>(value, label, itemElement)</code> when selection changes     </td></tr>
     *     <tr><td>    fill(items, selectedItem [, valField, labelField])     </td><td>     replaces all segments from an array     </td></tr>
     *     <tr><td>    disable([flg])     </td><td>     disables all segments (or the reverse if the optional argument is <code>false</code>)     </td></tr>
     *     <tr><td>    enable([flg])     </td><td>     enables all segments (or the reverse if the optional argument is <code>false</code>)     </td></tr>
     * </table>
     */
    static segmented_control() {}

    /**
     * This HTML tag, "search-input", creates a search field with a built-in clear button and optional result list.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 100px;">Attribute</th><th align="left">Description</th></tr>
     *     <tr><td>    placeholder="Search..."     </td><td>     placeholder text     </td></tr>
     *     <tr><td>    value="text"     </td><td>     initial value     </td></tr>
     *     <tr><td>    maxlength="100"     </td><td>     maximum input length     </td></tr>
     *     <tr><td>    disabled     </td><td>     disables the field     </td></tr>
     * </table>
     * <table>
     *     <tr><th align="left" style="padding-right: 120px;">API</th><th align="left">Description</th></tr>
     *     <tr><td>    getValue() / setValue(val) / clear()     </td><td>     manages the search text     </td></tr>
     *     <tr><td>    oninput(fun) / onsearch(fun) / onclear(fun)     </td><td>     registers field callbacks     </td></tr>
     *     <tr><td>    setResults(items [, options])     </td><td>     displays selectable results. Options include valueField, labelField, descriptionField, and metaField.     </td></tr>
     *     <tr><td>    onselect(fun)     </td><td>     receives <code>(value, label, item, element)</code> when a result is chosen     </td></tr>
     *     <tr><td>    showResults() / hideResults()     </td><td>     shows or hides the result list     </td></tr>
     *     <tr><td>    focus()     </td><td>     sets focus on the search field     </td></tr>
     *     <tr><td>    disable([flg])     </td><td>     disables the field (or the reverse if the optional argument is <code>false</code>)     </td></tr>
     *     <tr><td>    enable([flg])     </td><td>     enables the field (or the reverse if the optional argument is <code>false</code>)     </td></tr>
     * </table>
     */
    static search_input() {}

    /**
     * This HTML tag, "accordion", creates a reusable disclosure group.
     * <br><br>
     * Sections are declared with child <code>accordion-item title="..." value="...">Content</code> tags.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 100px;">Attribute</th><th align="left">Description</th></tr>
     *     <tr><td>    single     </td><td>     allows only one section open at a time     </td></tr>
     *     <tr><td>    persist-key="key"     </td><td>     stores open sections in localStorage     </td></tr>
     * </table>
     * <table>
     *     <tr><th align="left" style="padding-right: 120px;">API</th><th align="left">Description</th></tr>
     *     <tr><td>    open(value) / close(value) / toggle(value)     </td><td>     controls a section     </td></tr>
     *     <tr><td>    getOpenValues() / setOpen(values)     </td><td>     reads or sets all open sections     </td></tr>
     *     <tr><td>    onchange(fun)     </td><td>     receives <code>(value, open, openValues)</code> when a section changes     </td></tr>
     * </table>
     */
    static accordion() {}

    /**
     * This HTML tag, "badge-chip", creates a small status badge or chip.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 100px;">Attribute</th><th align="left">Description</th></tr>
     *     <tr><td>    tone="neutral"     </td><td>     neutral, accent, info, success, warning, error, or danger     </td></tr>
     *     <tr><td>    size="md"     </td><td>     sm, md, or lg     </td></tr>
     *     <tr><td>    icon="✓"     </td><td>     optional leading icon text     </td></tr>
     *     <tr><td>    dot     </td><td>     shows a leading status dot     </td></tr>
     * </table>
     * <table>
     *     <tr><th align="left" style="padding-right: 120px;">API</th><th align="left">Description</th></tr>
     *     <tr><td>    getValue() / setValue(text)     </td><td>     reads or replaces the badge label text     </td></tr>
     *     <tr><td>    setTone(tone)     </td><td>     changes the tone (neutral, accent, info, success, warning, error, or danger)     </td></tr>
     * </table>
     */
    static badge_chip() {}

    /**
     * This HTML tag, "avatar-badge", creates a generic avatar from initials or an image.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 100px;">Attribute</th><th align="left">Description</th></tr>
     *     <tr><td>    name="Jane Doe"     </td><td>     name used for initials and label     </td></tr>
     *     <tr><td>    src="image.png"     </td><td>     optional image URL     </td></tr>
     *     <tr><td>    size="40px"     </td><td>     avatar size     </td></tr>
     *     <tr><td>    tone="neutral"     </td><td>     neutral, accent, success, warning, error, or danger     </td></tr>
     *     <tr><td>    status="online"     </td><td>     optional status dot: online, away, busy, or error     </td></tr>
     * </table>
     * <table>
     *     <tr><th align="left" style="padding-right: 120px;">API</th><th align="left">Description</th></tr>
     *     <tr><td>    setName(name)     </td><td>     updates the name used for initials and label     </td></tr>
     *     <tr><td>    setSrc(url)     </td><td>     sets or replaces the avatar image     </td></tr>
     *     <tr><td>    setStatus(status)     </td><td>     updates the status dot (online, away, busy, or error)     </td></tr>
     * </table>
     */
    static avatar_badge() {}

    /**
     * This HTML tag, "toast", defines a reusable non-modal notification region.  Toasts may also be shown without
     * an explicit tag by calling <code>Utils.toast(message [, options])</code> or one of
     * <code>Utils.toast.success()</code>, <code>Utils.toast.info()</code>, <code>Utils.toast.warning()</code>, or
     * <code>Utils.toast.error()</code>.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 100px;">Attribute</th><th align="left">Description</th></tr>
     *     <tr><td>    position="top-center"     </td><td>     one of top-left, top-center, top-right, bottom-left, bottom-center, or bottom-right. Top positions sit below <code>.app-nav</code> when present.     </td></tr>
     *     <tr><td>    type="info"     </td><td>     default toast type: info, success, warning, or error     </td></tr>
     *     <tr><td>    duration="3000"     </td><td>     milliseconds before a toast closes; use 0 to keep it open until dismissed     </td></tr>
     *     <tr><td>    time="3000"     </td><td>     alias for duration     </td></tr>
     *     <tr><td>    auto-dismiss="false"     </td><td>     keep the toast open until it is dismissed manually     </td></tr>
     *     <tr><td>    manual     </td><td>     keep the toast open until it is dismissed manually     </td></tr>
     *     <tr><td>    max-visible="3"     </td><td>     maximum number of visible toasts in this region     </td></tr>
     *     <tr><td>    no-dismiss     </td><td>     hide the dismiss button     </td></tr>
     * </table>
     * <br>
     * <strong>Content</strong>
     * <br><br>
     *     If content is supplied, it is shown as an initial toast.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 120px;">API</th><th align="left">Description</th></tr>
     *     <tr><td>    show(message [, options])     </td><td>     displays a toast message and returns an object with <code>close()</code>     </td></tr>
     *     <tr><td>    success(message [, options])     </td><td>     displays a success toast     </td></tr>
     *     <tr><td>    info(message [, options])     </td><td>     displays an informational toast     </td></tr>
     *     <tr><td>    warning(message [, options])     </td><td>     displays a warning toast     </td></tr>
     *     <tr><td>    error(message [, options])     </td><td>     displays an error toast     </td></tr>
     *     <tr><td>    manual(message [, options])     </td><td>     displays a toast that stays visible until dismissed     </td></tr>
     *     <tr><td>    clear()     </td><td>     closes all toasts in this region     </td></tr>
     *     <tr><td>    configure(options)     </td><td>     updates default type, position, duration, maxVisible, or dismissible values     </td></tr>
     * </table>
     */
    static toast() {}

    /**
     * This HTML tag, "push-button", adds functionality and a consistent and convenient API to the HTML provided button input.
     * <br><br>
     *     No new attributes are defined.
     * <br><br>
     * <strong>Content</strong>
     * <br><br>
     *     No element content is defined.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 120px;">API</th><th align="left">Description</th></tr>
     *     <tr><td>    click()     </td><td>     simulate a button click               </td></tr>
     *     <tr><td>    disable([flg])     </td><td>     the control remains visible but inactive (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     *     <tr><td>    enable([flg])     </td><td>     the control is set to visible and enabled (or the reverse if the optional argument is <code>false</code>)              </td></tr>
     *     <tr><td>    focus()     </td><td>    sets focus to control           </td></tr>
     *     <tr><td>    getValue()     </td><td>     returns the label on the push button             </td></tr>
     *     <tr><td>    hide([flg])     </td><td>     hides the control (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     *     <tr><td>    isDisabled()     </td><td>     <code>true</code> if control is disabled               </td></tr>
     *     <tr><td>    isHidden()     </td><td>    <code>true</code> if control is hidden (not visible)           </td></tr>
     *     <tr><td>    isReadOnly()     </td><td>    <code>true</code> if control is read-only              </td></tr>
     *     <tr><td>    isVisible()     </td><td>    <code>true</code> if control is visible (not hidden)        </td></tr>
     *     <tr><td>    readOnly([flg])     </td><td>     sets control to read-only (or the reverse if the optional argument is <code>false</code>)              </td></tr>
     *     <tr><td>    readWrite([flg])     </td><td>    sets control to read-write (or the reverse if the optional argument is <code>false</code>)          </td></tr>
     *     <tr><td>    onclick(fun)     </td><td>     <code>fun</code> is executed when the user clicks on the button               </td></tr>
     *     <tr><td>    setValue(val)     </td><td>     sets the label on the push button              </td></tr>
     *     <tr><td>    show([flg])     </td><td>     the control is made visible (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     * </table>
     */
    static push_button() {}

    /**
     * This HTML tag, "radio-button", adds functionality and a consistent and convenient API to the HTML provided radio input element.
     * <br><br>
     * One thing that makes this control different from the others is that it can be referred to with the <code>$$</code> function
     * by its group name or the individual radio button id.  When the group name is used, the entire group is effected.  When an
     * individual radio button is addressed by its id, only that control is effected.
     * <br><br>
     * All the radio buttons in the same group should share the
     * same group name.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 100px;">Attribute</th><th align="left">Description</th></tr>
     *     <tr><td>    align-horizontal     </td><td>    align the buttons horizontally (default)             </td></tr>
     *     <tr><td>    align-vertical     </td><td>    align the buttons vertically         </td></tr>
     *     <tr><td>    button-style="style"     </td><td>    style used for the button portion of the radio button           </td></tr>
     *     <tr><td>    checked     </td><td>    pre-selects the particular radio button             </td></tr>
     *     <tr><td>    group="name"     </td><td>     the name of the group this radio button is a part of (the same for each radio button in a group)              </td></tr>
     *     <tr><td>    label-style="style"     </td><td>    style used for the label portion of the radio button          </td></tr>
     *     <tr><td>    name="name"     </td><td>     this is an alternate to the <code>group</code> attribute for HTML consistency               </td></tr>
     *     <tr><td>    required     </td><td>     a selection is required               </td></tr>
     *     <tr><td>    value="value"     </td><td>     required unique value associate with each radio button (different for each radio button)               </td></tr>
     * </table>
     * <br>
     * <strong>Content</strong>
     * <br><br>
     *     This is the label associated with the radio button.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 120px;">API</th><th align="left">Description</th></tr>
     * <table>
     *     <tr><th align="left" style="padding-right: 120px;">API</th><th align="left">Description</th></tr>
     *     <tr><td>    clear()     </td><td>    sets the radio button group to none selected   </td></tr>
     *     <tr><td>    disable([flg])     </td><td>    set the control to disabled (or the reverse if the optional argument is <code>false</code>)    </td></tr>
     *     <tr><td>    enable([flg])     </td><td>    set the control to enabled (or the reverse if the optional argument is <code>false</code>)            </td></tr>
     *     <tr><td>    focus()     </td><td>    set focus on the current control           </td></tr>
     *     <tr><td>    getIntValue()     </td><td>     the integer value of the selected ratio button group               </td></tr>
     *     <tr><td>    getValue()     </td><td>     the string value of the selected ratio button group               </td></tr>
     *     <tr><td>    hide([flg])     </td><td>    hides the control (or the reverse if the optional argument is <code>false</code>)             </td></tr>
     *     <tr><td>    isDirty()     </td><td>     <code>true</code> if the user changed the value             </td></tr>
     *     <tr><td>    isDisabled()     </td><td>     <code>true</code> if the control is disabled            </td></tr>
     *     <tr><td>    isError(desc)     </td><td>     used for error checking. If error, display error message and return <code>true</code>.  <code>desc</code> is a description of the user field.               </td></tr>
     *     <tr><td>    isHidden(desc)     </td><td>     <code>true</code> if the control is hidden (not visible)   </td></tr>
     *     <tr><td>    isReadOnly()     </td><td>     <code>true</code> if the control is read-only           </td></tr>
     *     <tr><td>    isVisible()     </td><td>     <code>true</code> if the control is visible (not hidden)           </td></tr>
     *     <tr><td>    onChange(fun)     </td><td>     execute <code>fun</code> whenever the state of this control changes.  <code>fun</code> is passed the value of the control group.  </td></tr>
     *     <tr><td>    readOnly([flg])     </td><td>    sets the control to read-only (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     *     <tr><td>    readWrite([flg])     </td><td>    sets the control to read-write (or the reverse if the optional argument is <code>false</code>)        </td></tr>
     *     <tr><td>    setLabel(val)     </td><td>     selects the label for the control               </td></tr>
     *     <tr><td>    setValue(val)     </td><td>     selects the button with the associated value               </td></tr>
     *     <tr><td>    show([flg])     </td><td>    sets the control to show (not hide) (or the reverse if the optional argument is <code>false</code>)           </td></tr>
     * </table>
     * </table>
     */
    static radio_button() {}

    /**
     * This HTML tag, "textbox-input", adds functionality and a consistent and convenient API to the HTML provided multi-line text input.
     *
     * Static HTML content can be used inside this control.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 100px;">Attribute</th><th align="left">Description</th></tr>
     *     <tr><td>    minlength="5"     </td><td>     sets the minimum acceptable string length               </td></tr>
     *     <tr><td>    maxlength="200"     </td><td>     sets the maximum number of characters               </td></tr>
     *     <tr><td>    placeholder="" </td><td>   text to be displaced in the control until the user enters data </td><tr>
     *     <tr><td>    required     </td><td>     an entry is required (at least 1 character)              </td></tr>
     *     <tr><td>    upcase     </td><td>     when the user enters text, it is auto-upcased               </td></tr>
     * </table>
     * <br>
     * <strong>Content</strong>
     * <br><br>
     *     The <em>Content</em> represents the placeholder or what is shown as a prompt inside the control when there is no value.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 120px;">API</th><th align="left">Description</th></tr>
     *     <tr><td>    clear()     </td><td>     erases the contents of the control               </td></tr>
     *     <tr><td>    disable([flg])     </td><td>     the control remains visible but inactive (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     *     <tr><td>    enable([flg])     </td><td>     the control is set to visible and enabled (or the reverse if the optional argument is <code>false</code>)              </td></tr>
     *     <tr><td>    focus()     </td><td>     sets the focus (where the cursor is located) to this control              </td></tr>
     *     <tr><td>    getValue()     </td><td>     returns the string associated with the control             </td></tr>
     *     <tr><td>    hide([flg])     </td><td>     the control is hidden (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     *     <tr><td>    isDirty()     </td><td>    <code>true</code> if user changed control contents             </td></tr>
     *     <tr><td>    isDisabled()     </td><td>    <code>true</code> if control is disabled            </td></tr>
     *     <tr><td>    isError(desc)     </td><td>     used for error checking. If error, display error message and return <code>true</code>.  <code>desc</code> is a description of the user field.               </td></tr>
     *     <tr><td>    isHidden()     </td><td>    <code>true</code> if user control is hidden (not visible)             </td></tr>
     *     <tr><td>    isReadOnly()     </td><td>    <code>true</code> if control is read-only             </td></tr>
     *     <tr><td>    isVisible()     </td><td>    <code>true</code> if control is visible (not hidden)          </td></tr>
     *     <tr><td>    onCChange(fun)     </td><td> execute <code>fun</code> immediately when the value is changed by the user, <code>fun</code> is passed the control value    </td></tr>
     *     <tr><td>    onChange(fun)     </td><td>     execute <code>fun</code>whenever the user exits the control if the value changed, <code>fun</code> is passed the control value               </td></tr>
     *     <tr><td>    readOnly([flg])     </td><td>     sets control to read-only (or the reverse if the optional argument is <code>false</code>)       </td></tr>
     *     <tr><td>    readWrite([flg])     </td><td>     sets control to read-write (or the reverse if the optional argument is <code>false</code>)     </td></tr>
     *     <tr><td>    setHtmlValue(val)     </td><td>     sets the string inside the control. The string is interpreted as HTML.             </td></tr>
     *     <tr><td>    setValue(val)     </td><td>     sets the string inside the control              </td></tr>
     *     <tr><td>    show([flg])     </td><td>     the control is made visible (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     * </table>
     */
    static textbox_input() {}

    /**
     * This HTML tag, "text-input", adds functionality and a consistent and convenient API to the HTML provided text input.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 100px;">Attribute</th><th align="left">Description</th></tr>
     *     <tr><td> fixcap </td><td> auto-correct capitalization of each word (first char uppercase, rest lowercase) </td></tr>
     *     <tr><td>    minlength="5"     </td><td>     sets the minimum acceptable string length               </td></tr>
     *     <tr><td>    maxlength="20"     </td><td>     sets the maximum number of characters               </td></tr>
     *     <tr><td>    password     </td><td>     the character are not shown on the screen              </td></tr>
     *     <tr><td>    required     </td><td>     an entry is required (at least 1 character)              </td></tr>
     *     <tr><td>    size="20"     </td><td>     width of control in number of characters (default 20)              </td></tr>
     *     <tr><td>    upcase     </td><td>     when the user enters text, it is auto-upcased               </td></tr>
     * </table>
     * <br>
     * <strong>Content</strong>
     * <br><br>
     *     The <em>Content</em> represents the placeholder or what is shown as a prompt inside the control when there is no value.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 120px;">API</th><th align="left">Description</th></tr>
     *     <tr><td>    clear()     </td><td>     erases the contents of the control               </td></tr>
     *     <tr><td>    disable([flg])     </td><td>     the control remains visible but inactive (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     *     <tr><td>    enable([flg])     </td><td>     the control is set to visible and enabled (or the reverse if the optional argument is <code>false</code>)              </td></tr>
     *     <tr><td>    focus()     </td><td>     sets the focus (where the cursor is located) to this control              </td></tr>
     *     <tr><td>    getValue()     </td><td>     returns the string associated with the control             </td></tr>
     *     <tr><td>    hide([flg])     </td><td>     the control is hidden (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     *     <tr><td>    isDirty()     </td><td>  has the control contents been changed by user      </td></tr>
     *     <tr><td>    isError(desc)     </td><td>     used for error checking. If error, display error message and return <code>true</code>.  <code>desc</code> is a description of the user field.               </td></tr>
     *     <tr><td>    isDisabled()     </td><td> is the control disabled? </td></tr>
     *     <tr><td>    isHidden()     </td><td> is the control hidden? </td></tr>
     *     <tr><td>    isReadOnly()     </td><td> is the control read-only?    </td></tr>
     *     <tr><td>    isVisible()     </td><td> is the control visible?   </td></tr>
     *     <tr><td>    onCChange(fun)     </td><td> execute <code>fun</code> immediately when the value is changed by the user, <code>fun</code> is passed the control value    </td></tr>
     *     <tr><td>    onChange(fun)     </td><td>     execute <code>fun</code>whenever the user exits the control if the value changed, <code>fun</code> is passed the control value               </td></tr>
     *     <tr><td>    onEnter(fun)      </td><td>  execute fun when enter key hit   </td></tr>
     *     <tr><td>    readOnly([flg])     </td><td> set control to read-only (or the reverse if the optional argument is <code>false</code>)    </td></tr>
     *     <tr><td>    readWrite([flg])     </td><td> set control to read-write (or the reverse if the optional argument is <code>false</code>)    </td></tr>
     *     <tr><td>    setPassword(val)     </td><td>     if <code>true</code>, treat as a password control; if <code>false</code>, treat as text input - previous value is returned   </td></tr>
     *     <tr><td>    setValue(val)     </td><td>     sets the string inside the control              </td></tr>
     *     <tr><td>    show([flg])     </td><td>     the control is made visible (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     * </table>
     */
    static text_input() {}

    /**
     * This HTML tag, "text-label", adds functionality and a consistent and convenient API to the HTML provided label tag when the 'for' attribute is
     * used or the 'span' tag otherwise.
     * <br><br>
     * <strong>Content</strong>
     * <br><br>
     *     The <em>Content</em> represents the content of the label.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 120px;">API</th><th align="left">Description</th></tr>
     *     <tr><td>    clear()     </td><td>     erases the contents of the control               </td></tr>
     *     <tr><td>    getValue()     </td><td>     returns the string associated with the control             </td></tr>
     *     <tr><td>    hide([flg])     </td><td>     the control is hidden (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     *     <tr><td>    isHidden()     </td><td> is the control hidden? </td></tr>
     *     <tr><td>    isVisible()     </td><td> is the control visible?   </td></tr>
     *     <tr><td>    onclick(fun)     </td><td>     <code>fun</code> is executed when the user clicks on the text   </td></tr>
     *     <tr><td>    setColor(val)     </td><td>     sets the color of the text              </td></tr>
     *     <tr><td>    setValue(val)     </td><td>     sets the string inside the control              </td></tr>
     *     <tr><td>    setHTMLValue(val)     </td><td>     sets the HTML inside the control              </td></tr>
     *     <tr><td>    show([flg])     </td><td>     the control is made visible (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     * </table>
     */
    static text_label() {}

    /**
     * This HTML tag, "time-input", provides a control where the user can enter a time.  The time appear like "3:30 PM".  A 24 hour clock is also supported automatically (like 14:30).
     * The values this control interacts with is a plain integer in the form HHMM in a 24 hour clock.  So, "1:30 PM" would be <code>1330</code>.
     * -1 is returned if the field is left blank.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 100px;">Attribute</th><th align="left">Description</th></tr>
     *     <tr><td>    min="0800"     </td><td>     the minimum time allowed (also accept times like "3:30 pm", etc.)              </td></tr>
     *     <tr><td>    min="1800"     </td><td>     the maximum time allowed  (also accept times like "3:30 pm", etc.)               </td></tr>
     *     <tr><td>    no-placeholder     </td><td>     do not display hh:mm when field is empty  </td></tr>
     *     <tr><td>    required     </td><td>     an entry is required               </td></tr>
     *     <tr><td>    size="20"     </td><td>     width of control in number of characters (default 20)              </td></tr>
     *     <tr><td>    zero-fill     </td><td>     zero fill the display               </td></tr>
     * </table>
     * <br>
     * <strong>Content</strong>
     * <br><br>
     *     The <em>Content</em> represents the HTML that would normally be inside an HTML <code>select</code> element.  This would only be used
     *     in cases of a static list.  List contents that depended on data would use the <code>add</code> method.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 120px;">API</th><th align="left">Description</th></tr>
     *     <tr><td>    clear()     </td><td>     remove the value associated with the control               </td></tr>
     *     <tr><td>    disable([flg])     </td><td>     the control remains visible but inactive (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     *     <tr><td>    enable([flg])     </td><td>     the control is set to visible and enabled (or the reverse if the optional argument is <code>false</code>)              </td></tr>
     *     <tr><td>    focus()     </td><td>     sets the focus (where the cursor is located) to this control              </td></tr>
     *     <tr><td>    getValue()     </td><td>     returns the value associated with the control             </td></tr>
     *     <tr><td>    hide([flg])     </td><td>     the control is hidden (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     *     <tr><td>    isDirty()     </td><td>     <code>true</code> if user changed control value     </td></tr>
     *     <tr><td>    isDisabled()     </td><td>     <code>true</code> if control is disabled    </td></tr>
     *     <tr><td>    isError(desc)     </td><td>     used for error checking. If error, display error message and return <code>true</code>.  <code>desc</code> is a description of the user field.               </td></tr>
     *     <tr><td>    isHidden()     </td><td>     <code>true</code> if control is hidden (not visible)     </td></tr>
     *     <tr><td>    isReadOnly()     </td><td>     <code>true</code> if control is read-only     </td></tr>
     *     <tr><td>    isVisible()     </td><td>     <code>true</code> if control is visible (not hidden)    </td></tr>
     *     <tr><td>    onCChange(fun)     </td><td> execute <code>fun</code> immediately when the value is changed by the user, <code>fun</code> is passed the control value    </td></tr>
     *     <tr><td>    onChange(fun)     </td><td>     execute <code>fun</code>whenever the user exits the control if the value changed, <code>fun</code> is passed the control value               </td></tr>
     *     <tr><td>    onEnter(fun)      </td><td>  execute fun when enter key hit   </td></tr>
     *     <tr><td>    readOnly([flg])     </td><td>   sets control to read-only (or the reverse if the optional argument is <code>false</code>)   </td></tr>
     *     <tr><td>    readWrite([flg])     </td><td>    sets control to read-write (or the reverse if the optional argument is <code>false</code>)      </td></tr>
     *     <tr><td>    setMaxValue(val)     </td><td>     sets the maximum value the control will accept  (accept times like 1530, "3:30 pm", etc.)           </td></tr>
     *     <tr><td>    setMinValue(val)     </td><td>     sets the minimum value the control will accept   (accept times like 1530, "3:30 pm", etc.)             </td></tr>
     *     <tr><td>    setValue(val)     </td><td>     sets the value associated with the control              </td></tr>
     *     <tr><td>    show([flg])     </td><td>     the control is made visible (or the reverse if the optional argument is <code>false</code>)               </td></tr>
     * </table>
     */
    static time_input() {}

}
