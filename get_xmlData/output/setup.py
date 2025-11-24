# setup.py
from setuptools import setup
from Cython.Build import cythonize
import cython

setup(
    ext_modules = cythonize("timeseries_cy.pyx", compiler_directives={'language_level' : "3"})
)