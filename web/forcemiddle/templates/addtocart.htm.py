from mako import runtime, filters, cache
UNDEFINED = runtime.UNDEFINED
_magic_number = 1
_modified_time = 1200639893.493762
_template_filename=u'templates/addtocart.htm'
_template_uri=u'/addtocart.htm'
_template_cache=cache.Cache(__name__, _modified_time)
_exports = []


def _mako_get_namespace(context, name):
    try:
        return context.namespaces[(__name__, name)]
    except KeyError:
        _mako_generate_namespaces(context)
        return context.namespaces[(__name__, name)]
def _mako_generate_namespaces(context):
    # SOURCE LINE 1
    ns = runtime.Namespace('__anon_0x208a7f0', context._clean_inheritance_tokens(), templateuri=u'teampage.mak', callables=None, calling_uri=_template_uri, module=None)
    context.namespaces[(__name__, '__anon_0x208a7f0')] = ns

def render_body(context,**pageargs):
    __locals = dict(pageargs=pageargs)
    _import_ns = {}
    _mako_get_namespace(context, '__anon_0x208a7f0')._populate(_import_ns, [u'item_name', u'sku'])
    item_name = _import_ns.get('item_name', context.get('item_name', UNDEFINED))
    context.write(u'\n<form target="paypal" action="https://www.paypal.com/cgi-bin/webscr" method="post">\n<p class="addtocart">\n    <input type="image" \n           src="https://www.paypal.com/en_US/i/btn/btn_cart_SM.gif" \n           border="0" name="submit" \n           alt="Make payments with PayPal - it\'s fast, free and secure!">\n</p>\n<input type="hidden" name="add" value="1">\n<input type="hidden" name="cmd" value="_cart">\n<input type="hidden" name="business" value="forcemiddlediscs@gmail.com">\n<input type="hidden" name="item_name" value="')
    # SOURCE LINE 12
    context.write(unicode(item_name))
    context.write(u'">\n<input type="hidden" name="item_number" value="Jojah-2008">\n<input type="hidden" name="amount" value="12.00">\n<input type="hidden" name="no_shipping" value="0">\n<input type="hidden" name="no_note" value="1">\n<input type="hidden" name="currency_code" value="USD">\n<input type="hidden" name="lc" value="US">\n<input type="hidden" name="bn" value="PP-ShopCartBF">\n</form>\n')
    return ''


