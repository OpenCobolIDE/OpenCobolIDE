"""
This module contains a function that can be used to init os.environ to be
ready to run msvc based compiler process (highly windows specific).
"""
import logging
import subprocess
import os


#: environment variables we are intetrested in
INTERESTING = set(("include", "lib", "libpath", "path"))


def initialize(vcvarsall, arch='x86'):
    """
    Modifies os.environ with the VC vars.

    :param vcvarsall: path to the vcvarsall batch to run.
    :param arch: architecture to setup (x86 or x64).
    """
    try:
        vc_env = query_vcvarsall(vcvarsall, arch)
    except RuntimeError:
        _logger().exception('failed to initialize VC vars, compilation will '
                            'likely not work...')
    else:
        for key in INTERESTING:
            dst_key = key
            if key == 'path':
                dst_key = key.upper()
            try:
                os.environ[dst_key] = vc_env[key]
            except KeyError:
                _logger().warn('key error: %s', key)
            else:
                _logger().debug('%s=%s', dst_key, os.environ[dst_key])


def query_vcvarsall(path, arch):
    """
    Launch vcvarsall.bat for the given architecture and reads the environment
    variables from the standard output.

    This function has been taken from distutils2 (and adapted for own needs).
    """
    result = {}
    _logger().debug('querying vcvarsall: "%s" %s set', path, arch)
    popen = subprocess.Popen('"%s" %s & set' % (path, arch),
                             stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE)
    stdout, stderr = popen.communicate()
    if popen.wait() != 0:
        raise RuntimeError(stderr.decode("mbcs"))

    def convert_mbcs(s):
        dec = getattr(s, "decode", None)
        if dec is not None:
            try:
                s = dec("mbcs")
            except UnicodeError:
                pass
        return s

    def rm_duplicates(variable):
        """Remove duplicate values of an environment variable.
        """
        oldList = variable.split(os.pathsep)
        newList = []
        for i in oldList:
            if i not in newList:
                newList.append(i)
        newVariable = os.pathsep.join(newList)
        return newVariable

    stdout = stdout.decode("mbcs")
    for line in stdout.split("\n"):
        line = convert_mbcs(line)
        if '=' not in line:
            continue
        line = line.strip()
        key, value = line.split('=', 1)
        key = key.lower()
        if key in INTERESTING:
            if value.endswith(os.pathsep):
                value = value[:-1]
            result[key] = rm_duplicates(value)
    return result


def _logger():
    return logging.getLogger(__name__)
