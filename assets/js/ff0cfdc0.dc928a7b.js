(self.webpackChunkdocs=self.webpackChunkdocs||[]).push([[454],{3905:function(e,t,n){"use strict";n.d(t,{Zo:function(){return u},kt:function(){return m}});var r=n(7294);function s(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function a(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function o(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?a(Object(n),!0).forEach((function(t){s(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):a(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function i(e,t){if(null==e)return{};var n,r,s=function(e,t){if(null==e)return{};var n,r,s={},a=Object.keys(e);for(r=0;r<a.length;r++)n=a[r],t.indexOf(n)>=0||(s[n]=e[n]);return s}(e,t);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);for(r=0;r<a.length;r++)n=a[r],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(s[n]=e[n])}return s}var l=r.createContext({}),c=function(e){var t=r.useContext(l),n=t;return e&&(n="function"==typeof e?e(t):o(o({},t),e)),n},u=function(e){var t=c(e.components);return r.createElement(l.Provider,{value:t},e.children)},p={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},d=r.forwardRef((function(e,t){var n=e.components,s=e.mdxType,a=e.originalType,l=e.parentName,u=i(e,["components","mdxType","originalType","parentName"]),d=c(n),m=s,h=d["".concat(l,".").concat(m)]||d[m]||p[m]||a;return n?r.createElement(h,o(o({ref:t},u),{},{components:n})):r.createElement(h,o({ref:t},u))}));function m(e,t){var n=arguments,s=t&&t.mdxType;if("string"==typeof e||s){var a=n.length,o=new Array(a);o[0]=d;var i={};for(var l in t)hasOwnProperty.call(t,l)&&(i[l]=t[l]);i.originalType=e,i.mdxType="string"==typeof e?e:s,o[1]=i;for(var c=2;c<a;c++)o[c]=n[c];return r.createElement.apply(null,o)}return r.createElement.apply(null,n)}d.displayName="MDXCreateElement"},767:function(e,t,n){"use strict";n.r(t),n.d(t,{frontMatter:function(){return i},contentTitle:function(){return l},metadata:function(){return c},toc:function(){return u},default:function(){return d}});var r=n(2122),s=n(9756),a=(n(7294),n(3905)),o=["components"],i={title:"Assertions",sidebar_label:"Assertions"},l=void 0,c={unversionedId:"assertions",id:"assertions",isDocsHomePage:!1,title:"Assertions",description:"In the Test module, we provide very basic assertion material.",source:"@site/docs/assertions.md",sourceDirName:".",slug:"/assertions",permalink:"/rescript-test/assertions",editUrl:"https://github.com/bloodyowl/rescript-test/edit/master/website/docs/assertions.md",version:"current",frontMatter:{title:"Assertions",sidebar_label:"Assertions"},sidebar:"docs",previous:{title:"API",permalink:"/rescript-test/api"},next:{title:"Test output",permalink:"/rescript-test/test-output"}},u=[{value:"assertion",id:"assertion",children:[]},{value:"throws",id:"throws",children:[]},{value:"doesNotThrow",id:"doesnotthrow",children:[]},{value:"pass",id:"pass",children:[]},{value:"fail",id:"fail",children:[]},{value:"todo",id:"todo",children:[]}],p={toc:u};function d(e){var t=e.components,n=(0,s.Z)(e,o);return(0,a.kt)("wrapper",(0,r.Z)({},p,n,{components:t,mdxType:"MDXLayout"}),(0,a.kt)("p",null,"In the ",(0,a.kt)("inlineCode",{parentName:"p"},"Test")," module, we provide very basic assertion material. "),(0,a.kt)("p",null,"The framework ",(0,a.kt)("strong",{parentName:"p"},"doesn't handle comparisons for you"),", you'll need to provide your comparator functions. The goal is to avoid expensive recursive comparisons as much as possible (while still letting you do them if necessary)."),(0,a.kt)("h2",{id:"assertion"},"assertion"),(0,a.kt)("p",null,"Assertion is the basic building block, here's its signature:"),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-js"},"assertion(\n  // comparison function, returns a boolean\n  comparator,\n  // the value you're testing\n  a,\n  // the value you're expecting\n  b,\n  // optional, gives a name in case of test failure\n  ~operator: string=?,\n  // optional, gives a message on the test line\n  ~message: string=?,\n)\n")),(0,a.kt)("p",null,"Let's write a simple assertion:"),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-js"},'assertion(\n  (a, b) => a === b,\n  1,\n  1,\n  ~operator="Int equals",\n  ~message="One equals one",\n)\n')),(0,a.kt)("p",null,"You can see how this can be a bit redundant. What we recommend is to create an ",(0,a.kt)("inlineCode",{parentName:"p"},"Assert.res")," file containing the most common assertions:"),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-js",metastring:'title="Assert.res"',title:'"Assert.res"'},'let intEqual = (~message=?, a: int, b: int) =>\n  assertion(~message?, ~operator="Int equals", (a, b) => a === b, a, b)\n')),(0,a.kt)("p",null,"And reuse them across your tests:"),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-js",metastring:'title="Add_test.res"',title:'"Add_test.res"'},'open Test\nopen Assert\n\ntest("Add", () => {\n  intEquals(add(1, 1), 2)\n  intEquals(~message="1 + 2 === 3", add(1, 2), 3)\n})\n')),(0,a.kt)("h2",{id:"throws"},"throws"),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-js",metastring:'title="TestForException_test.res"',title:'"TestForException_test.res"'},'test("Test with a function that\'s supposed to throw", () => {\n  throws(() => {\n    myFunction()\n  })\n\n  throws(() => {\n    myFunction()\n  }, ~message="myFunction throws")\n\n  throws(\n    () => myFunction(),\n    ~message="myFunction throws",\n    ~test=exn => {\n      // test than exn is the one expected\n    },\n  )\n})\n')),(0,a.kt)("h2",{id:"doesnotthrow"},"doesNotThrow"),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-js",metastring:'title="TestForException_test.res"',title:'"TestForException_test.res"'},'test("Test with a function that\'s not supposed to throw", () => {\n  doesNotThrow(() => {\n    myFunction()\n  })\n\n  doesNotThrow(() => {\n    myFunction()\n  }, ~message="myFunction doesn\'t throw")\n})\n')),(0,a.kt)("h2",{id:"pass"},"pass"),(0,a.kt)("p",null,"When you simply need to validate than a certain code path is reached, asserting a ",(0,a.kt)("inlineCode",{parentName:"p"},"pass")," is enough:"),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-js",metastring:'title="TestWithPass_test.res"',title:'"TestWithPass_test.res"'},'test("Test with pass", () => {\n  myFunction(() => {\n    pass()\n  })\n  myFunction(() => {\n    pass(~message="Callback is reached", ())\n  })\n})\n')),(0,a.kt)("h2",{id:"fail"},"fail"),(0,a.kt)("p",null,"The other way around, when you want to assert that you don't reach a certain path:"),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-js",metastring:'title="TestWithFail_test.res"',title:'"TestWithFail_test.res"'},'test("Test with fail", () => {\n  myFunction(() => {\n    fail()\n  })\n  myFunction(() => {\n    fail(~message="Callback is not reached", ())\n  })\n})\n')),(0,a.kt)("h2",{id:"todo"},"todo"),(0,a.kt)("p",null,"If you're in a hurry and want to remember you'll eventually need to write this test:"),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-js",metastring:'title="TestForLater_test.res"',title:'"TestForLater_test.res"'},'test("Test with todo", () => {\n  todo("Check that int overflows are handled")\n})\n')))}d.isMDXComponent=!0}}]);