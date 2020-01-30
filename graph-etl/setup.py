from setuptools import setup


setup(
    name="gdan_tmp_etl",
    version="0.1.0",
    description="",
    long_description="",
    long_description_content_type='text/markdown',
    url="https://github.com/bmeg/gdan-tmp-etl",
    license="MIT",
    packages=["etl"],
    package_data={"": ["schema/*.yaml"]},
    zip_safe=False,
    python_requires=">=3.7, <4",
    install_requires=[
        "dictionaryutils==2.0.7",
        "jsonschema>=3.0.1",
        "requests>=2.18.1"
    ],
    extras_require={
        "test": [
            "nose>=1.3.7",
            "flake8>=3.5.0",
        ]
    },
    dependency_links=[
        "git+https://github.com/uc-cdis/dictionaryutils.git@2.0.7"
    ],
    classifiers=[
        "Development Status :: 3 - Alpha",
        "Intended Audience :: Developers",
        "Natural Language :: English",
        "License :: OSI Approved :: MIT License",
        "Topic :: Software Development :: Libraries",
        "Programming Language :: Python :: 3.7",
    ],
)
