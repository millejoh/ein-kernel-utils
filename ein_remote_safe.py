"""
Python code to support ein-kernel-utils.el

Copyright (C) 2012 - Takafumi Arakaki
Copyright (C) 2020 - John Miller

Author: Takafumi Arakaki <aka.tkf at gmail.com>
Author: John Miller

ein_remote_safe.py is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

ein_remote_safe.py is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with ein_remote_safe.py.  If not, see <http://www.gnu.org/licenses/>.

"""

__ein_pytools_version = "1.3.0"

try:
    from matplotlib import rc as __ein_rc
    from matplotlib import rcParams as __ein_rcParams
    __ein_matplotlib_available = True
except ImportError:
    __ein_matplotlib_available = False


def __ein_export_nb(nb_json, format):
    import IPython.nbconvert as nbconvert
    import IPython.nbformat as nbformat
    nb = nbformat.reads(nb_json, nbformat.NO_CONVERT)
    output = nbconvert.export_by_name(format, nb)
    print(output[0])

def __ein_find_edit_target_python(name):
    from jedi import Interpreter

    try:
        defs = Interpreter(name, [globals(), locals()]).infer()
        if defs:
            return(defs[0].module_path, defs[0].line)
        else:
            return False
    except:
        return False

def __ein_find_source(name):
    """Given an object as string, `name`, return its place in source code."""
    import json

    ret =  __ein_find_edit_target_python(name)
    if ret:
        (filename, lineno) = ret
        print(json.dumps({'filename':filename,
                          'lineno':lineno}))
    else:
        raise RuntimeError("Source code for {0} cannot be found".format(name))


def __ein_run_docstring_examples(obj, verbose=True):
    from IPython.core.interactiveshell import InteractiveShell
    import doctest
    inst = InteractiveShell.instance()
    globs = inst.user_ns
    return doctest.run_docstring_examples(obj, globs, verbose=verbose)

def __ein_generate_oinfo_data(ostrings):
    import json

    safe_strings = [o for o in ostrings if not o.startswith('%')]
    odata = [__ein_object_info_for(obj) for obj in safe_strings]

    print (json.dumps(odata))
    return odata

def __ein_object_info_for(name):
    from jedi import Interpreter

    try:
        inferred = Interpreter(name, [globals(), locals()]).infer()
        defs = inferred[0]
        sig = defs.get_signatures()
        info_dict = {'type': defs.type,
                     'module_name': defs.module_name,
                     'module_path': defs.module_path,
                     'line': defs.line,
                     'column': defs.column,
                     'docstring': defs.docstring(),
                     'full_name' : defs.full_name,
                     'call_signature' : ''
                     }
        if (type(sig) is list):
            info_dict['call_signature'] = sig[0].to_string()
        return info_dict
    except Exception:
        return {'type': '',
                'module_name': '',
                'line': '',
                'column': '',
                'docstring': '',
                'full_name': '',
                'call_signature': '',
                }

def __ein_print_object_info_for(obj):
    import json

    print (json.dumps(__ein_object_info_for(obj)))

def __ein_eval_hy_string(obj):
    try:
        import hy
    except ImportError:
        print("Hy not supported in this kernel. Execute `pip install hy` if you want this support.")

    expr = hy.read_str(obj)
    ret = hy.eval(expr)

    return ret

def __ein_set_matplotlib_param(family, setting, value):
    settings = {}
    if __ein_matplotlib_available:
        settings[setting] = eval(value)
        __ein_rc(family, **settings)
    else:
        raise RuntimeError("Matplotlib not installed in this instance of python!")


def __ein_set_figure_size(dim):
    __ein_set_matplotlib_param('figure', 'figsize', dim)


def __ein_set_figure_dpi(dpi):
    __ein_set_matplotlib_param('figure', 'dpi', dpi)


def __ein_get_matplotlib_params():
    if __ein_matplotlib_available:
        import json
        print(json.dumps([k for k in __ein_rcParams.keys()]))
    else:
        raise RuntimeError("Matplotlib not installed in this instance of python!")
