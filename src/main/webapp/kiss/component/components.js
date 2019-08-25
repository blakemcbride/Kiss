
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
 *  All of the controls support all of the standard HTML global attributes.  The following lists some of the attributes commonly used:
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
     *     <tr><td>    disable()     </td><td>     the control remains visible but inactive               </td></tr>
     *     <tr><td>    enable()     </td><td>     the control is set to visible and enabled              </td></tr>
     *     <tr><td>    focus()     </td><td>     sets focus on control            </td></tr>
     *     <tr><td>    getValue()     </td><td>     returns <code>true</code> if checked and <code>false</code> if unchecked               </td></tr>
     *     <tr><td>    hide()     </td><td>     the control is hidden               </td></tr>
     *     <tr><td>    isDirty()     </td><td>    true if the user changed its state     </td></tr>
     *     <tr><td>    isDisabled()     </td><td>    true if the control is disabled     </td></tr>
     *     <tr><td>    isHidden()     </td><td>    true if the control is hidden (not visible)    </td></tr>
     *     <tr><td>    isReadOnly()     </td><td>    true if the control is read-only     </td></tr>
     *     <tr><td>    isVisible()     </td><td>    true if the control is visible (not hidden)     </td></tr>
     *     <tr><td>    onChange(fun)     </td><td>     execute <code>fun</code>whenever the state of this control changes.               </td></tr>
     *     <tr><td>    readOnly()     </td><td>    sets control to read-only         </td></tr>
     *     <tr><td>    readWrite()     </td><td>    sets control to read-write        </td></tr>
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
     *     <tr><td>    max="20181231"     </td><td>     the maximum date allowed               </td></tr>
     *     <tr><td>    min="20180101"     </td><td>     the minimum date allowed               </td></tr>
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
     *     <tr><td>    disable()     </td><td>     the control remains visible but inactive               </td></tr>
     *     <tr><td>    enable()     </td><td>     the control is set to visible and enabled              </td></tr>
     *     <tr><td>    focus()     </td><td>     sets the focus (where the cursor is located) to this control              </td></tr>
     *     <tr><td>    getDateValue()     </td><td>     returns the date as a <code>Date</code> instance             </td></tr>
     *     <tr><td>    getIntValue()     </td><td>     returns the date as an integer with the "YYYYMMDD" format             </td></tr>
     *     <tr><td>    getSQLValue()     </td><td>     returns the date as a string with the "YYYY-MM-DD" format             </td></tr>
     *     <tr><td>    hide()     </td><td>     the control is hidden               </td></tr>
     *     <tr><td>    isDirty()     </td><td>    true if the user changed the value       </td></tr>
     *     <tr><td>    isDisabled()     </td><td>    true if the control is disabled       </td></tr>
     *     <tr><td>    isError(desc)     </td><td>     used for error checking. If error, display error message and return <code>true</code>.  <code>desc</code> is a description of the user field.               </td></tr>
     *     <tr><td>    isHidden()     </td><td>   true if the control is hidden (not visible)    </td></tr>
     *     <tr><td>    isReadOnly()     </td><td>   true if the control is read-only    </td></tr>
     *     <tr><td>    isVisible()     </td><td>   true if the control is visible (not hidden)    </td></tr>
     *     <tr><td>    onChange(fun)     </td><td>     execute <code>fun</code>whenever the state of this control changes.               </td></tr>
     *     <tr><td>    readOnly()     </td><td>    set control to read-only  </td></tr>
     *     <tr><td>    readWrite()     </td><td>    set control to read-write     </td></tr>
     *     <tr><td>    setValue(val)     </td><td>     sets the control value.  <code>val</code> may be a <code>Date</code>, <code>number</code> (20180608), or <code>string</code> ("2018-06-08")              </td></tr>
     *     <tr><td>    show()     </td><td>     the control is made visible               </td></tr>
     * </table>
     */
    static date_input() {}

    /**
     * This HTML tag, "drop-down", adds functionality and a consistent and convenient API to the HTML provided <code>select</code> tag.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 100px;">Attribute</th><th align="left">Description</th></tr>
     *     <tr><td>    default-option="label"     </td><td>     what is shown before the user makes a selection.  This would often be something like "(choose)"               </td></tr>
     *     <tr><td>    required     </td><td>     an entry is required               </td></tr>
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
     *     <tr><td>    clear()     </td><td>     remove the list contents except the <code>default-option</code>               </td></tr>
     *     <tr><td>    disable()     </td><td>     the control remains visible but inactive               </td></tr>
     *     <tr><td>    enable()     </td><td>     the control is set to visible and enabled              </td></tr>
     *     <tr><td>    focus()     </td><td>    sets focus on the control        </td></tr>
     *     <tr><td>    getData()     </td><td>     returns the data associated to the selected option             </td></tr>
     *     <tr><td>    getLabel()     </td><td>     returns the label associated to the selected option             </td></tr>
     *     <tr><td>    getValue()     </td><td>     returns the value associated to the selected option             </td></tr>
     *     <tr><td>    hide()     </td><td>     the control is hidden               </td></tr>
     *     <tr><td>    isDirty()     </td><td>     true if user changed value      </td></tr>
     *     <tr><td>    isDisabled()     </td><td>     true if control is disabled      </td></tr>
     *     <tr><td>    isHidden()     </td><td>     true if control is hidden (not visible)      </td></tr>
     *     <tr><td>    isError(desc)     </td><td>     used for error checking. If error, display error message and return <code>true</code>.  <code>desc</code> is a description of the user field.               </td></tr>
     *     <tr><td>    isReadOnly()     </td><td>     true if control is read-only     </td></tr>
     *     <tr><td>    isVisible()     </td><td>     true if control is visible (not hidden)      </td></tr>
     *     <tr><td>    onChange(fun)     </td><td>     execute <code>fun</code>whenever the state of this control changes. <code>fun</code> is called as follows <code>fun(val, lbl, data)</code>              </td></tr>
     *     <tr><td>    readOnly()     </td><td>    make control read-only      </td></tr>
     *     <tr><td>    readWrite()     </td><td>    make control read-write      </td></tr>
     *     <tr><td>    setValue(val)     </td><td>     selects the row that contains the value specified by <code>val</code>              </td></tr>
     *     <tr><td>    show()     </td><td>     the control is made visible               </td></tr>
     *     <tr><td>    size()     </td><td>    returns the number of rows in the list (including <code>default-option</code>              </td></tr>
     * </table>
     */
    static drop_down() {}

    /**
     * This HTML tag, "list-box", adds functionality and a consistent and convenient API to the HTML provided <code>select</code> tag.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 100px;">Attribute</th><th align="left">Description</th></tr>
     *     <tr><td>    default-option="label"     </td><td>     what is the default selection     </td></tr>
     *     <tr><td>    multiple     </td><td>     multiple entries may be selected (an array will be returned)               </td></tr>
     *     <tr><td>    required     </td><td>     an entry is required               </td></tr>
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
     *     <tr><td>    clear()     </td><td>     remove the list contents except the <code>default-option</code>               </td></tr>
     *     <tr><td>    disable()     </td><td>     the control remains visible but inactive               </td></tr>
     *     <tr><td>    enable()     </td><td>     the control is set to visible and enabled              </td></tr>
     *     <tr><td>    focus()     </td><td>    sets focus on control            </td></tr>
     *     <tr><td>    getData()     </td><td>     returns the data associated to the selected option             </td></tr>
     *     <tr><td>    getLabel()     </td><td>     returns the label associated to the selected option             </td></tr>
     *     <tr><td>    getValue()     </td><td>     returns the value associated to the selected option (returns an array if <code>multiple</code> attribute included)            </td></tr>
     *     <tr><td>    hide()     </td><td>     the control is hidden               </td></tr>
     *     <tr><td>    isDirty()     </td><td>    true if user has changed control value     </td></tr>
     *     <tr><td>    isDisabled()     </td><td>    true if control is disabled     </td></tr>
     *     <tr><td>    isError(desc)     </td><td>     used for error checking. If error, display error message and return <code>true</code>.  <code>desc</code> is a description of the user field.               </td></tr>
     *     <tr><td>    isHidden()     </td><td>    true if control is hidden (not visible)     </td></tr>
     *     <tr><td>    isReadOnly()     </td><td>     true if control is read-only     </td></tr>
     *     <tr><td>    isVisible()     </td><td>    true if control is visible (not hidden)     </td></tr>
     *     <tr><td>    onClick(fun)     </td><td>     execute <code>fun</code>whenever the user clicks on an item. <code>fun</code> is called as follows <code>fun(val, lbl, data)</code>              </td></tr>
     *     <tr><td>    onDblClick(fun)     </td><td>     execute <code>fun</code>whenever the user double-clicks on an item. <code>fun</code> is called as follows <code>fun(val, lbl, data)</code>              </td></tr>
     *     <tr><td>    readOnly()     </td><td>     sets control to read-only    </td></tr>
     *     <tr><td>    readWrite()     </td><td>     sets control to read-write    </td></tr>
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
     *     <tr><td>    decimal-places="2"     </td><td>     controls the maximum number of digits past the decimal point (default 0) </td></tr>
     *     <tr><td>    dollar-sign     </td><td>     adds a dollar sign when formatting the number               </td></tr>
     *     <tr><td>    min="20"     </td><td>     sets the minimum acceptable value (default 0)   </td></tr>
     *     <tr><td>    max="200"     </td><td>     sets the maximum acceptable value               </td></tr>
     *     <tr><td>    money     </td><td>     sets <code>min="0" dollar-sign decimal-places="2"</code>               </td></tr>
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
     *     <tr><td>    disable()     </td><td>     the control remains visible but inactive               </td></tr>
     *     <tr><td>    enable()     </td><td>     the control is set to visible and enabled              </td></tr>
     *     <tr><td>    focus()     </td><td>     sets the focus (where the cursor is located) to this control              </td></tr>
     *     <tr><td>    getValue()     </td><td>     returns the numeric value of the control             </td></tr>
     *     <tr><td>    hide()     </td><td>     the control is hidden               </td></tr>
     *     <tr><td>    isDirty()     </td><td>     did user change control content?   </td></tr>
     *     <tr><td>    isDisabled()     </td><td>     is control disabled?   </td></tr>
     *     <tr><td>    isError(desc)     </td><td>     used for error checking. If error, display error message and return <code>true</code>.  <code>desc</code> is a description of the user field.               </td></tr>
     *     <tr><td>    isHidden()     </td><td>     is control hidden?   </td></tr>
     *     <tr><td>    isReadOnly()     </td><td>     is control read-only?    </td></tr>
     *     <tr><td>    isVisible()     </td><td>     is control visible?   </td></tr>
     *     <tr><td>    readOnly()     </td><td> set control to read-only    </td></tr>
     *     <tr><td>    readWrite()     </td><td> set control to read-write    </td></tr>
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
     *     <tr><td>    focus()     </td><td>    sets focus to control           </td></tr>
     *     <tr><td>    getValue()     </td><td>     returns the label on the push button             </td></tr>
     *     <tr><td>    hide()     </td><td>     hides the control               </td></tr>
     *     <tr><td>    isDisabled()     </td><td>     true if control is disabled               </td></tr>
     *     <tr><td>    isHidden()     </td><td>    true if control is hidden (not visible)           </td></tr>
     *     <tr><td>    isReadOnly()     </td><td>    true if control is read-only              </td></tr>
     *     <tr><td>    isVisible()     </td><td>    true if control is visible (not hidden)        </td></tr>
     *     <tr><td>    readOnly()     </td><td>     sets control to read-only              </td></tr>
     *     <tr><td>    readWrite()     </td><td>    sets control to read-write          </td></tr>
     *     <tr><td>    onclick(fun)     </td><td>     <code>fun</code> is executed when the user clicks on the button               </td></tr>
     *     <tr><td>    setValue(val)     </td><td>     sets the label on the push button              </td></tr>
     *     <tr><td>    show()     </td><td>     the control is made visible               </td></tr>
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
     *     <tr><td>    group="name"     </td><td>     the name of the group this radio button is a part of (the same for each radio button in a group)              </td></tr>
     *     <tr><td>    checked     </td><td>    pre-selects the particular radio button             </td></tr>
     *     <tr><td>    label-style="style"     </td><td>    style used for the label           </td></tr>
     *     <tr><td>    name="name"     </td><td>     this is an alternate to the <code>group</code> attribute for HTML consistency               </td></tr>
     *     <tr><td>    required     </td><td>     an entry is required               </td></tr>
     *     <tr><td>    value="name"     </td><td>     required unique value associate with each radio button (different for each radio button)               </td></tr>
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
     *     <tr><td>    disable()     </td><td>    set the control to disabled    </td></tr>
     *     <tr><td>    enable()     </td><td>    set the control to enabled            </td></tr>
     *     <tr><td>    focus()     </td><td>    set focus on the current control           </td></tr>
     *     <tr><td>    getValue()     </td><td>     the value of the selected ratio button group               </td></tr>
     *     <tr><td>    hide()     </td><td>    hides the control             </td></tr>
     *     <tr><td>    isDirty()     </td><td>     true if the user changed the value             </td></tr>
     *     <tr><td>    isDisabled()     </td><td>     true if the control is disabled            </td></tr>
     *     <tr><td>    isError(desc)     </td><td>     used for error checking. If error, display error message and return <code>true</code>.  <code>desc</code> is a description of the user field.               </td></tr>
     *     <tr><td>    isHidden(desc)     </td><td>     true if the control is hidden (not visible)   </td></tr>
     *     <tr><td>    isReadOnly()     </td><td>     true if the control is read-only           </td></tr>
     *     <tr><td>    isVisible()     </td><td>     true if the control is visible (not hidden)           </td></tr>
     *     <tr><td>    onChange(fun)     </td><td>     execute <code>fun</code>whenever the state of this control changes.               </td></tr>
     *     <tr><td>    readOnly()     </td><td>    sets the control to read-only               </td></tr>
     *     <tr><td>    readWrite()     </td><td>    sets the control to read-write        </td></tr>
     *     <tr><td>    setValue(val)     </td><td>     selects the button with the associated value               </td></tr>
     *     <tr><td>    show()     </td><td>    sets the control to show (not hide)           </td></tr>
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
     *     <tr><td>    rows="20"     </td><td>     sets the <em>minimum</em> number of visible rows (it will expand to fill the area it is in) </td></tr>
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
     *     <tr><td>    disable()     </td><td>     the control remains visible but inactive               </td></tr>
     *     <tr><td>    enable()     </td><td>     the control is set to visible and enabled              </td></tr>
     *     <tr><td>    focus()     </td><td>     sets the focus (where the cursor is located) to this control              </td></tr>
     *     <tr><td>    getValue()     </td><td>     returns the string associated with the control             </td></tr>
     *     <tr><td>    hide()     </td><td>     the control is hidden               </td></tr>
     *     <tr><td>    isDirty()     </td><td>    true if user changed control contents             </td></tr>
     *     <tr><td>    isDisabled()     </td><td>    true if control is disabled            </td></tr>
     *     <tr><td>    isError(desc)     </td><td>     used for error checking. If error, display error message and return <code>true</code>.  <code>desc</code> is a description of the user field.               </td></tr>
     *     <tr><td>    isHidden()     </td><td>    true if user control is hidden (not visible)             </td></tr>
     *     <tr><td>    isReadOnly()     </td><td>    true if control is read-only             </td></tr>
     *     <tr><td>    isVisible()     </td><td>    true if control is visible (not hidden)          </td></tr>
     *     <tr><td>    onChange(fun)     </td><td>  execute fun when control changes              </td></tr>
     *     <tr><td>    onKeyDown(fun)     </td><td> execute fun when key down    </td></tr>
     *     <tr><td>    readOnly()     </td><td>     sets control to read-only       </td></tr>
     *     <tr><td>    readWrite()     </td><td>     sets control to read-write     </td></tr>
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
     *     <tr><td>    disable()     </td><td>     the control remains visible but inactive               </td></tr>
     *     <tr><td>    enable()     </td><td>     the control is set to visible and enabled              </td></tr>
     *     <tr><td>    focus()     </td><td>     sets the focus (where the cursor is located) to this control              </td></tr>
     *     <tr><td>    getValue()     </td><td>     returns the string associated with the control             </td></tr>
     *     <tr><td>    hide()     </td><td>     the control is hidden               </td></tr>
     *     <tr><td>    isDirty()     </td><td>  has the control contents been changed by user      </td></tr>
     *     <tr><td>    isError(desc)     </td><td>     used for error checking. If error, display error message and return <code>true</code>.  <code>desc</code> is a description of the user field.               </td></tr>
     *     <tr><td>    isDisabled()     </td><td> is the control disabled? </td></tr>
     *     <tr><td>    isHidden()     </td><td> is the control hidden? </td></tr>
     *     <tr><td>    isReadOnly()     </td><td> is the control read-only?    </td></tr>
     *     <tr><td>    isVisible()     </td><td> is the control visible?   </td></tr>
     *     <tr><td>    onChange(fun)     </td><td>  execute fun when control changes              </td></tr>
     *     <tr><td>    onKeyDown(fun)     </td><td> execute fun when key down    </td></tr>
     *     <tr><td>    readOnly()     </td><td> set control to read-only    </td></tr>
     *     <tr><td>    readWrite()     </td><td> set control to read-write    </td></tr>
     *     <tr><td>    setValue(val)     </td><td>     sets the string inside the control              </td></tr>
     *     <tr><td>    show()     </td><td>     the control is made visible               </td></tr>
     * </table>
     */
    static text_input() {}

    /**
     * This HTML tag, "text-label", adds functionality and a consistent and convenient API to the HTML provided label tag.
     * <br><br>
     * <strong>Content</strong>
     * <br><br>
     *     The <em>Content</em> represents the content of the label.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 120px;">API</th><th align="left">Description</th></tr>
     *     <tr><td>    clear()     </td><td>     erases the contents of the control               </td></tr>
     *     <tr><td>    getValue()     </td><td>     returns the string associated with the control             </td></tr>
     *     <tr><td>    hide()     </td><td>     the control is hidden               </td></tr>
     *     <tr><td>    isHidden()     </td><td> is the control hidden? </td></tr>
     *     <tr><td>    isVisible()     </td><td> is the control visible?   </td></tr>
      *    <tr><td>    setValue(val)     </td><td>     sets the string inside the control              </td></tr>
     *     <tr><td>    show()     </td><td>     the control is made visible               </td></tr>
     * </table>
     */
    static text_label() {}

    /**
     * This HTML tag, "time-input", provides a control where the user can enter a time.  The time appear like "3:30 PM".  A 24 hour clock is also supported automatically (like 14:30).
     * The values this control interacs with is a plain integer in the form HHMM in a 24 hour clock.  So, "1:30 PM" would be <code>1330</code>.
     * <br><br>
     * <table>
     *     <tr><th align="left" style="padding-right: 100px;">Attribute</th><th align="left">Description</th></tr>
     *     <tr><td>    min="0800"     </td><td>     the minimum time allowed               </td></tr>
     *     <tr><td>    min="1800"     </td><td>     the maximum time allowed               </td></tr>
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
     *     <tr><td>    disable()     </td><td>     the control remains visible but inactive               </td></tr>
     *     <tr><td>    enable()     </td><td>     the control is set to visible and enabled              </td></tr>
     *     <tr><td>    focus()     </td><td>     sets the focus (where the cursor is located) to this control              </td></tr>
     *     <tr><td>    getValue()     </td><td>     returns the value associated with the control             </td></tr>
     *     <tr><td>    hide()     </td><td>     the control is hidden               </td></tr>
     *     <tr><td>    isDirty()     </td><td>     true if user changed control value     </td></tr>
     *     <tr><td>    isDisabled()     </td><td>     true if control is disabled    </td></tr>
     *     <tr><td>    isError(desc)     </td><td>     used for error checking. If error, display error message and return <code>true</code>.  <code>desc</code> is a description of the user field.               </td></tr>
     *     <tr><td>    isHidden()     </td><td>     true if control is hidden (not visible)     </td></tr>
     *     <tr><td>    isReadOnly()     </td><td>     true if control is read-only     </td></tr>
     *     <tr><td>    isVisible()     </td><td>     true if control is visible (not hidden)    </td></tr>
     *     <tr><td>    readOnly()     </td><td>   sets control to read-only   </td></tr>
     *     <tr><td>    readWrite()     </td><td>    sets control to read-write      </td></tr>
     *     <tr><td>    setValue(val)     </td><td>     sets the value associated with the control              </td></tr>
     *     <tr><td>    show()     </td><td>     the control is made visible               </td></tr>
     * </table>
     */
    static time_input() {}

}
