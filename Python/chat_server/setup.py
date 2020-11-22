from setuptools import setup
from setuptools import find_packages

setup(
    name='chat-server',
    version='1.0.0',
    description='Simple socket chat server',
    author='Alex Bennett',
    packages=find_packages(exclude=('tests*', 'testing*')),
)
