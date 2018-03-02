// ***********************************************************************
// Assembly         : TypeTestsFromCSharp
// Author           : chrisgk
// Created          : 03-01-2018
//
// Last Modified By : chrisgk
// Last Modified On : 03-01-2018
// ***********************************************************************
// <copyright file="RFC7950-Example-System.cs" company="">
//     Copyright ©  2018
// </copyright>
// <summary>
//  Tests that the types generated in F# can be consumed by C#.
//  This file contains tests for the simple example from RFC 7950.
// </summary>
// ***********************************************************************

namespace Yang.Examples.CSharp.Tests
{
    using Microsoft.VisualStudio.TestTools.UnitTesting;
    using RFC7950.SimpleModel;

    /// <summary>
    /// Class Rfc7950ExampleSystem.
    /// </summary>
    [TestClass]
    public class Rfc7950ExampleSystem
    {
        /// <summary>
        /// Tests the reading header information.
        /// </summary>
        [TestMethod]
        public void TestReadingHeaderInformation()
        {
            Assert.AreEqual("sys", Model.T.Information.Prefix);
            Assert.AreEqual("Example Inc.", Model.T.Information.Organization);
            Assert.AreEqual("joe@example.com", Model.T.Information.Contact);
            Assert.AreEqual("The module for entities implementing the Example system.", Model.T.Information.Description);
        }
    }
}
