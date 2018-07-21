
/* This file is only used for documentation purposes.  */


/**
 * This class doesn't actually exist.  It is merely used to group and document the HTML components that come with the <em>Kiss</em> system.
 * Each method represents a new HTML tag offered by the <em>Kiss</em> system.  Although these methods are shown with an underscore in their name,
 * the actual tag they represent would have a dash instead.
 * <br><br>
 * In addition to the additional capabilities offered by some of these controls, many are enhancements over features already provided by HTML.
 * These enhancements offer significant additioanl functionality as well as a more consistent interface.
 * <br><br>
 * In addition to the tag attributes and content documented here, most of these new HTML tags also support the standard attributes
 * that HTML normally supports (such as "style" and "id").  Each of these controls (except <code>radio-button</code>) <em>require</em> the "id" attribute in order
 * for your code to reference the control.
 * <br><br>
 * Components are accessed via the <code>$$</code> global function.  For example, if you have HTML containing:
 * <br><br>
 *     &nbsp;&nbsp;&nbsp;&nbsp; <code>&lt;text-input id="fname"&gt;&lt;/text-input&gt;</code>
 *  <br><br>
 *  You would acces that control in your code with <code>$$('fname')</code>  Methods available withing that control are
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
 *
 *
 */
class components {

    /**
     * There is no constructor.
     */
    constructor() {}

    /**
     * This HTML tag, "check-box", adds functionality and a consistent and convenient API to the HTML provided checkbox.
     * <br><br>
     * <strong>Attributes</strong>
     * <br><br>
     *     This element contains no attributes in addition to those supplied by an HTML checkbox input type.
     * <br><br>
     * <strong>Content</strong>
     * <br><br>
     *     The <em>Content</em> represents the label assoiated with the checkbox.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 120px;">API</th><th align="left">Description</th></tr>
     *     <tr><td>    clear()     </td><td>     uncheck the box               </td></tr>
     *     <tr><td>    disable()     </td><td>     the control remains visible but inactive               </td></tr>
     *     <tr><td>    enable()     </td><td>     the control is set to visible and enabled              </td></tr>
     *     <tr><td>    getValue()     </td><td>     returns <code>true</code> if checked and <code>false</code> if unchecked               </td></tr>
     *     <tr><td>    hide()     </td><td>     the control is hidden               </td></tr>
     *     <tr><td>    onChange(fun)     </td><td>     execute <code>fun</code>whenever the state of this control changes.               </td></tr>
     *     <tr><td>    setValue(val)     </td><td>     if <code>val</code> is <code>true</code> check the box, uncheck if <code>false</code>               </td></tr>
     *     <tr><td>    show()     </td><td>     the control is made visible               </td></tr>
     * </table>
     */
    static check_box() {}

    /**
     * This HTML tag, "date-input", adds functionality and a consistent and convenient API to the HTML provided date input.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 100px;">Attribute</th><th align="left">Description</th></tr>
     *     <tr><td>    min="20180101"     </td><td>     the minimum date allowed               </td></tr>
     *     <tr><td>    max="20181231"     </td><td>     the maximum date allowed               </td></tr>
     *     <tr><td>    required     </td><td>     an entry is required               </td></tr>
     * </table>
     * <br><br>
     * <strong>Content</strong>
     * <br><br>
     *     The <em>Content</em> represents the placeholder or what is shown as a prompt inside the control when there is no value.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 120px;">API</th><th align="left">Description</th></tr>
     *     <tr><td>    clear()     </td><td>     clear the control value               </td></tr>
     *     <tr><td>    disable()     </td><td>     the control remains visible but inactive               </td></tr>
     *     <tr><td>    enable()     </td><td>     the control is set to visible and enabled              </td></tr>
     *     <tr><td>    focus()     </td><td>     sets the focus (where the cursor is located) to this control              </td></tr>
     *     <tr><td>    getDateValue()     </td><td>     returns the date as a <code>Date</code> instance             </td></tr>
     *     <tr><td>    getIntValue()     </td><td>     returns the date as an integer with the "YYYYMMDD" format             </td></tr>
     *     <tr><td>    getSQLValue()     </td><td>     returns the date as a string with the "YYYY-MM-DD" format             </td></tr>
     *     <tr><td>    hide()     </td><td>     the control is hidden               </td></tr>
     *     <tr><td>    isError(desc)     </td><td>     used for error checking. If error, display error message and return <code>true</code>.  <code>desc</code> is a description of the user field.               </td></tr>
     *     <tr><td>    onChange(fun)     </td><td>     execute <code>fun</code>whenever the state of this control changes.               </td></tr>
     *     <tr><td>    setValue(val)     </td><td>     sets the control value.  <code>val</code> may be a <code>Date</code>, <code>number</code> (20180608), or <code>string</code> ("2018-06-08")              </td></tr>
     *     <tr><td>    show()     </td><td>     the control is made visible               </td></tr>
     * </table>
     */
    static date_input() {}

    /**
     * This HTML tag, "drop-down", adds functionality and a consistent and convenient API to the HTML provided select.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 100px;">Attribute</th><th align="left">Description</th></tr>
     *     <tr><td>    default-option="label"     </td><td>     what is shown before the user makes a selection.  This would often be something like "(choose)"               </td></tr>
     *     <tr><td>    required     </td><td>     an entry is required               </td></tr>
     * </table>
     * <br><br>
     * <strong>Content</strong>
     * <br><br>
     *     The <em>Content</em> represents the HTML that would normally be inside an HTML <code>select</code> element.  This would only be used
     *     in cases of a static list.  List contents that depended on data would use the <code>add</code> method.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 120px;">API</th><th align="left">Description</th></tr>
     *     <tr><td>    add(val, lbl, data)     </td><td>     add a new list item.  <code>val</code> is the value associated to the option, <code>lbl</code> is the text shown in the list, and <code>data</data> represents optional and arbitrary data associated to the option               </td></tr>
     *     <tr><td>    clear()     </td><td>     remove the list contents except the <code>default-option</code>               </td></tr>
     *     <tr><td>    disable()     </td><td>     the control remains visible but inactive               </td></tr>
     *     <tr><td>    enable()     </td><td>     the control is set to visible and enabled              </td></tr>
     *     <tr><td>    getData()     </td><td>     returns the data associated to the selected option             </td></tr>
     *     <tr><td>    getLabel()     </td><td>     returns the label associated to the selected option             </td></tr>
     *     <tr><td>    getValue()     </td><td>     returns the value associated to the selected option             </td></tr>
     *     <tr><td>    hide()     </td><td>     the control is hidden               </td></tr>
     *     <tr><td>    isError(desc)     </td><td>     used for error checking. If error, display error message and return <code>true</code>.  <code>desc</code> is a description of the user field.               </td></tr>
     *     <tr><td>    onChange(fun)     </td><td>     execute <code>fun</code>whenever the state of this control changes. <code>fun</code> is called as follows <code>fun(val, lbl, data)</code>              </td></tr>
     *     <tr><td>    setValue(val)     </td><td>     selects the row that contains the value specified by <code>val</code>              </td></tr>
     *     <tr><td>    show()     </td><td>     the control is made visible               </td></tr>
     *     <tr><td>    size()     </td><td>    returns the number of rows in the list (including <code>default-option</code>              </td></tr>
     * </table>
     */
    static drop_down() {}

    /**
     * This HTML tag, "list-box", adds functionality and a consistent and convenient API to the HTML provided select.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 100px;">Attribute</th><th align="left">Description</th></tr>
     *     <tr><td>    required     </td><td>     an entry is required               </td></tr>
     * </table>
     * <br><br>
     * <strong>Content</strong>
     * <br><br>
     *     The <em>Content</em> represents the HTML that would normally be inside an HTML <code>select</code> element.  This would only be used
     *     in cases of a static list.  List contents that depended on data would use the <code>add</code> method.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 120px;">API</th><th align="left">Description</th></tr>
     *     <tr><td>    add(val, lbl, data)     </td><td>     add a new list item.  <code>val</code> is the value associated to the option, <code>lbl</code> is the text shown in the list, and <code>data</data> represents optional and arbitrary data associated to the option               </td></tr>
     *     <tr><td>    clear()     </td><td>     remove the list contents except the <code>default-option</code>               </td></tr>
     *     <tr><td>    disable()     </td><td>     the control remains visible but inactive               </td></tr>
     *     <tr><td>    enable()     </td><td>     the control is set to visible and enabled              </td></tr>
     *     <tr><td>    getData()     </td><td>     returns the data associated to the selected option             </td></tr>
     *     <tr><td>    getLabel()     </td><td>     returns the label associated to the selected option             </td></tr>
     *     <tr><td>    getValue()     </td><td>     returns the value associated to the selected option             </td></tr>
     *     <tr><td>    hide()     </td><td>     the control is hidden               </td></tr>
     *     <tr><td>    isError(desc)     </td><td>     used for error checking. If error, display error message and return <code>true</code>.  <code>desc</code> is a description of the user field.               </td></tr>
     *     <tr><td>    onClick(fun)     </td><td>     execute <code>fun</code>whenever the user clicks on an item. <code>fun</code> is called as follows <code>fun(val, lbl, data)</code>              </td></tr>
     *     <tr><td>    onDblClick(fun)     </td><td>     execute <code>fun</code>whenever the user double-clicks on an item. <code>fun</code> is called as follows <code>fun(val, lbl, data)</code>              </td></tr>
     *     <tr><td>    removeByIndex(idx)     </td><td>     remove the indicated row              </td></tr>
     *     <tr><td>    setValue(val)     </td><td>     selects the row that contains the value specified by <code>val</code>              </td></tr>
     *     <tr><td>    selectedIndex()     </td><td>     returns the index of the selected item  (-1 if none)             </td></tr>
     *     <tr><td>    show()     </td><td>     the control is made visible               </td></tr>
     *     <tr><td>    size()     </td><td>    returns the number of rows in the list               </td></tr>
     * </table>
     */
    static list_box() {}

    /**
     * This HTML tag, "numeric-input", adds functionality and a consistent and convenient API to the HTML input text element. For example, it will
     * only accept numbers, can verify decimal places, and formats the number when the control is exited by the user.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 100px;">Attribute</th><th align="left">Description</th></tr>
     *     <tr><td>    decimal-places="2"     </td><td>     controls the maximum number of digits past the decimal point               </td></tr>
     *     <tr><td>    dollar-sign     </td><td>     adds a dollar sign when formatting the number               </td></tr>
     *     <tr><td>    min="20"     </td><td>     sets the minimum acceptable value               </td></tr>
     *     <tr><td>    max="200"     </td><td>     sets the maximum acceptable value               </td></tr>
     *     <tr><td>    money     </td><td>     sets <code>min="o" dollar-sign decimal-places="2"</code>               </td></tr>
     *     <tr><td>    required     </td><td>     an entry is required               </td></tr>
     *     <tr><td>    show-zero     </td><td>    show zero values (instead of blank if zero)               </td></tr>
     * </table>
     * <br><br>
     * <strong>Content</strong>
     * <br><br>
     *     The <em>Content</em> represents the placeholder or what is shown as a prompt inside the control when there is no value.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 120px;">API</th><th align="left">Description</th></tr>
     *     <tr><td>    clear()     </td><td>     erases the contents of the control               </td></tr>
     *     <tr><td>    disable()     </td><td>     the control remains visible but inactive               </td></tr>
     *     <tr><td>    enable()     </td><td>     the control is set to visible and enabled              </td></tr>
     *     <tr><td>    focus()     </td><td>     sets the focus (where the cursor is located) to this control              </td></tr>
     *     <tr><td>    getValue()     </td><td>     returns the numeric value of the control             </td></tr>
     *     <tr><td>    hide()     </td><td>     the control is hidden               </td></tr>
     *     <tr><td>    isError(desc)     </td><td>     used for error checking. If error, display error message and return <code>true</code>.  <code>desc</code> is a description of the user field.               </td></tr>
     *     <tr><td>    setValue(val)     </td><td>     sets the numeric value of the control              </td></tr>
     *     <tr><td>    show()     </td><td>     the control is made visible               </td></tr>
     * </table>
     */
    static numeric_input() {}

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
     *     <tr><td>    disable()     </td><td>     the control remains visible but inactive               </td></tr>
     *     <tr><td>    enable()     </td><td>     the control is set to visible and enabled              </td></tr>
     *     <tr><td>    getValue()     </td><td>     returns the label on the push button             </td></tr>
     *     <tr><td>    hide()     </td><td>     the control is hidden               </td></tr>
     *     <tr><td>    onclick(fun)     </td><td>     <code>fun</code> is executed when the user clicks on the button               </td></tr>
     *     <tr><td>    setValue(val)     </td><td>     sets the label on the push button              </td></tr>
     *     <tr><td>    show()     </td><td>     the control is made visible               </td></tr>
     * </table>
     */
    static push_button() {}

    /**
     * This HTML tag, "check-box", adds functionality and a consistent and convenient API to the HTML provided radio input element.
     * <br><br>
     * One thing that makes this control different from the others is that it is referred to with the <code>$$</code> function
     * by its group name rather than the element <code>id</code>.  All the radio buttons in the same group shouls share the
     * same group name.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 100px;">Attribute</th><th align="left">Description</th></tr>
     *     <tr><td>    group="name"     </td><td>     the name of the group this radio button is a part of (the same for each radio button in a group)              </td></tr>
     *     <tr><td>    name="name"     </td><td>     this is an alternate to the <code>group</code> attribute for HTML consistency               </td></tr>
     *     <tr><td>    required     </td><td>     an entry is required               </td></tr>
     *     <tr><td>    value="name"     </td><td>     required unique value associate with each radio button (different for each radio button)               </td></tr>
     * </table>
     * <br><br>
     * <strong>Content</strong>
     * <br><br>
     *     This is the label associated with the radio button.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 120px;">API</th><th align="left">Description</th></tr>
     * <table>
     *     <tr><th align="left" style="padding-right: 120px;">API</th><th align="left">Description</th></tr>
     *     <tr><td>    getValue()     </td><td>     the value of the selected ratio button               </td></tr>
     *     <tr><td>    isError(desc)     </td><td>     used for error checking. If error, display error message and return <code>true</code>.  <code>desc</code> is a description of the user field.               </td></tr>
     *     <tr><td>    onChange(fun)     </td><td>     execute <code>fun</code>whenever the state of this control changes.               </td></tr>
     *     <tr><td>    setValue(val)     </td><td>     selects the button with the associated value               </td></tr>
     * </table>
     * </table>
     */
    static radio_button() {}

    /**
     * This HTML tag, "textbox-input", adds functionality and a consistent and convenient API to the HTML provided multi-line text input.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 100px;">Attribute</th><th align="left">Description</th></tr>
     *     <tr><td>    minlength="5"     </td><td>     sets the minimum acceptable string length               </td></tr>
     *     <tr><td>    maxlength="200"     </td><td>     sets the maximum number of characters               </td></tr>
     *     <tr><td>    password     </td><td>     the character are not shown on the screen              </td></tr>
     *     <tr><td>    required     </td><td>     an entry is required (at least 1 character)              </td></tr>
     *     <tr><td>    upcase     </td><td>     when the user enters text, it is auto-upcased               </td></tr>
     * </table>
     * <br><br>
     * <strong>Content</strong>
     * <br><br>
     *     The <em>Content</em> represents the placeholder or what is shown as a prompt inside the control when there is no value.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 120px;">API</th><th align="left">Description</th></tr>
     *     <tr><td>    clear()     </td><td>     erases the contents of the control               </td></tr>
     *     <tr><td>    disable()     </td><td>     the control remains visible but inactive               </td></tr>
     *     <tr><td>    enable()     </td><td>     the control is set to visible and enabled              </td></tr>
     *     <tr><td>    focus()     </td><td>     sets the focus (where the cursor is located) to this control              </td></tr>
     *     <tr><td>    getValue()     </td><td>     returns the string associated with the control             </td></tr>
     *     <tr><td>    hide()     </td><td>     the control is hidden               </td></tr>
     *     <tr><td>    isError(desc)     </td><td>     used for error checking. If error, display error message and return <code>true</code>.  <code>desc</code> is a description of the user field.               </td></tr>
     *     <tr><td>    setValue(val)     </td><td>     sets the string inside the control              </td></tr>
     *     <tr><td>    show()     </td><td>     the control is made visible               </td></tr>
     * </table>
     */
    static textbox_input() {}

    /**
     * This HTML tag, "text-input", adds functionality and a consistent and convenient API to the HTML provided text input.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 100px;">Attribute</th><th align="left">Description</th></tr>
     *     <tr><td>    minlength="5"     </td><td>     sets the minimum acceptable string length               </td></tr>
     *     <tr><td>    maxlength="20"     </td><td>     sets the maximum number of characters               </td></tr>
     *     <tr><td>    password     </td><td>     the character are not shown on the screen              </td></tr>
     *     <tr><td>    required     </td><td>     an entry is required (at least 1 character)              </td></tr>
     *     <tr><td>    upcase     </td><td>     when the user enters text, it is auto-upcased               </td></tr>
     * </table>
     * <br><br>
     * <strong>Content</strong>
     * <br><br>
     *     The <em>Content</em> represents the placeholder or what is shown as a prompt inside the control when there is no value.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 120px;">API</th><th align="left">Description</th></tr>
     *     <tr><td>    clear()     </td><td>     erases the contents of the control               </td></tr>
     *     <tr><td>    disable()     </td><td>     the control remains visible but inactive               </td></tr>
     *     <tr><td>    enable()     </td><td>     the control is set to visible and enabled              </td></tr>
     *     <tr><td>    focus()     </td><td>     sets the focus (where the cursor is located) to this control              </td></tr>
     *     <tr><td>    getValue()     </td><td>     returns the string associated with the control             </td></tr>
     *     <tr><td>    hide()     </td><td>     the control is hidden               </td></tr>
     *     <tr><td>    isError(desc)     </td><td>     used for error checking. If error, display error message and return <code>true</code>.  <code>desc</code> is a description of the user field.               </td></tr>
     *     <tr><td>    setValue(val)     </td><td>     sets the string inside the control              </td></tr>
     *     <tr><td>    show()     </td><td>     the control is made visible               </td></tr>
     * </table>
     */
    static text_input() {}

    /**
     * This HTML tag, "time-input", provides a control where the user can enter a time.  The time appear like "3:30 PM".  A 24 hour clock is also supported automatically (like 14:30).
     * The values this control interacs with is a plain integer in the form HHMM in a 24 hour clock.  So, "1:30 PM" would be <code>1330</code>.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 100px;">Attribute</th><th align="left">Description</th></tr>
     *     <tr><td>    min="0800"     </td><td>     the minimum time allowed               </td></tr>
     *     <tr><td>    min="1800"     </td><td>     the maximum time allowed               </td></tr>
     *     <tr><td>    required     </td><td>     an entry is required               </td></tr>
     *     <tr><td>    zero-fill     </td><td>     zero fill the display               </td></tr>
     * </table>
     * <br><br>
     * <strong>Content</strong>
     * <br><br>
     *     The <em>Content</em> represents the HTML that would normally be inside an HTML <code>select</code> element.  This would only be used
     *     in cases of a static list.  List contents that depended on data would use the <code>add</code> method.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 120px;">API</th><th align="left">Description</th></tr>
     *     <tr><td>    clear()     </td><td>     remove the value associated with the control               </td></tr>
     *     <tr><td>    disable()     </td><td>     the control remains visible but inactive               </td></tr>
     *     <tr><td>    enable()     </td><td>     the control is set to visible and enabled              </td></tr>
     *     <tr><td>    focus()     </td><td>     sets the focus (where the cursor is located) to this control              </td></tr>
     *     <tr><td>    getValue()     </td><td>     returns the value associated with the control             </td></tr>
     *     <tr><td>    hide()     </td><td>     the control is hidden               </td></tr>
     *     <tr><td>    isError(desc)     </td><td>     used for error checking. If error, display error message and return <code>true</code>.  <code>desc</code> is a description of the user field.               </td></tr>
     *     <tr><td>    setValue(val)     </td><td>     sets the value associated with the control              </td></tr>
     *     <tr><td>    show()     </td><td>     the control is made visible               </td></tr>
     * </table>
     */
    static time_input() {}

}