from setuptools import setup, find_packages

setup(
    name="cobol_splitter",
    version="0.1.0",
    packages=find_packages(),
    install_requires=[
        "aiofiles>=0.8.0",
        "tqdm>=4.62.0",
        "pytest>=7.0.0",
    ],
    entry_points={
        "console_scripts": [
            "cobol-splitter = cobol_splitter.cli:main",
        ],
    },
    author="Your Name",
    description="A tool to split and analyze COBOL programs",
    long_description=open("README.md").read(),
    long_description_content_type="text/markdown",
    python_requires=">=3.8",
)