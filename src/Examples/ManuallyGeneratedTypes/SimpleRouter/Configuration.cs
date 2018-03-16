// ***********************************************************************
// Assembly         : SimpleRouter
// Author           : chrisgk
// Created          : 03-15-2018
//
// Last Modified By : chrisgk
// Last Modified On : 03-15-2018
// ***********************************************************************
// <copyright file="Configuration.cs" company="">
//     Copyright ©  2018
// </copyright>
// <summary>
// Example of how a generated type should look like.
// This corresponds to the YANG model in src\misc\SimpleSchemas\Juniper-Interfaces-serial.yang,
// and it is just one way to represent the type in .NET.
// </summary>
// ***********************************************************************

// TODO: Should we make the generated simple type members readonly?
//       This should also solve the problem of getting consistent hash codes.

namespace SimpleRouter
{
    using System;
    using System.Collections.Generic;
    using System.Collections.ObjectModel;
    using System.Diagnostics.Contracts;
    using System.Text;
    using System.Text.RegularExpressions;
    using Yang.Generator;

    // Observe:
    // - transformations in name.
    // - initialization of members in place
    // - flattening of namespace

    /// <summary>
    /// Information about the generated module
    /// </summary>
    public struct ModuleInformation
    {
        /// <summary>
        /// The organization
        /// </summary>
        public static string Organization = "Juniper Networks, Inc.";
        /// <summary>
        /// The description
        /// </summary>
        public static string Description = "Junos YANG module for configuration hierarchies";
    }

    /// <summary>
    /// Class Configuration.
    /// </summary>
    public class Configuration
    {
        /// <summary>
        /// Is configuration state?
        /// </summary>
        public static bool Config = true;

        /// <summary>
        /// Software version information
        /// </summary>
        public string Version;

        /// <summary>
        /// Physical interface
        /// </summary>
        public class InterfaceClass
        {
            // Corresponds to: configuration/system/interfaces/interface

            /// <summary>
            /// Interface name
            /// </summary>
            public readonly string Name;

            /// <summary>
            /// Text description of interface
            /// </summary>
            public readonly string Description;

            /// <summary>
            /// Maximum transmission unit packet size
            /// </summary>
            private readonly UInt32 Mtu;

            private static bool _mtu_Check(UInt32 value) => value >= 256 && value <= 9216;
            private static string _mtu_Range = "256 .. 9216";

            /// <summary>
            /// Logical interface
            /// </summary>
            public class UnitClass : IEquatable<UnitClass>
            {
                public readonly string Name;

                /// <summary>
                /// Text description of interface
                /// </summary>
                public readonly string Description;

                public class VlanChoiceClass
                {
                    public enum VlanChoiceCase
                    {
                        /// <summary>
                        /// The vlan choice case invalid
                        /// </summary>
                        VlanChoiceCaseInvalid,

                        /// <summary>
                        /// Virtual LAN identifier value for 802.1q VLAN tags
                        /// </summary>
                        Case1,

                        /// <summary>
                        /// Virtual LAN identifier range of form vid1-vid2
                        /// </summary>
                        Case2
                    }

                    public VlanChoiceCase VlanChoice = VlanChoiceCase.VlanChoiceCaseInvalid;

                    private string _vlanId = default(string);

                    public string VlanId
                    {
                        get
                        {
                            if (VlanChoice == VlanChoiceCase.Case1)
                            {
                                return _vlanId;
                            }
                            else
                            {
                                // TODO: pick a proper exception type when attempting to access invalid enum
                                throw new Exception("Attempting to access variable corresponding to wrong case.");
                            }
                        }
                        set
                        {
                            VlanChoice = VlanChoiceCase.Case1;
                            _vlanId = value;
                        }
                    }

                    private string _vlanIdRange = default(string);

                    // ReSharper disable once InconsistentNaming
                    private static string _validIdRange_Pattern =
                        "^(([1-9][0-9]{0,2}|[1-3][0-9]{3}|40[0-8][0-9]|409[0-4])-([1-9][0-9]{0,2}|[1-3][0-9]{3}|40[0-8][0-9]|409[0-4]))$";

                    // ReSharper disable once InconsistentNaming
                    private static readonly Regex _validIdRange_RE = new Regex(_validIdRange_Pattern, RegexOptions.Compiled);

                    public string VlanIdRange
                    {
                        get
                        {
                            if (VlanChoice == VlanChoiceCase.Case1)
                            {
                                return _vlanIdRange;
                            }
                            else
                            {
                                // TODO: pick a proper exception type when attempting to access invalid enum
                                throw new Exception("Attempting to access variable corresponding to wrong case.");
                            }
                        }
                        set
                        {
                            if (_validIdRange_RE.IsMatch(value))
                            {
                                VlanChoice = VlanChoiceCase.Case1;
                                _vlanIdRange = value;
                            }
                            else
                            {
                                // Error message comes from model
                                throw new ArgumentOutOfRangeException(nameof(value), "Must be a string in the format <1-4094>-<1-4094>");
                            }
                        }
                    }
                }

                public readonly VlanChoiceClass VlanChoice;

                /// <summary>
                /// IPv4 parameters
                /// </summary>
                public class InetClass : IEquatable<InetClass>
                {
                    // Corresponds to unit/family/inet

                    /// <summary>
                    /// Interface address/destination prefix
                    /// </summary>
                    public class AddressClass : IEquatable<AddressClass>
                    {
                        /// <summary>
                        /// Interface address/destination prefix
                        /// </summary>
                        public readonly string Name;

                        public AddressClass(string name)
                        {
                            // We need to make sure that name is not null, because it is used as a key.

                            Contract.Requires(null != name);
                            if (name is null)
                            {
                                throw new ArgumentNullException(nameof(name));
                            }

                            this.Name = name;
                        }

                        public AddressClass(XmlHelper.XmlReaderHelper reader)
                        {
                            Contract.Requires(null != reader);
                            if (Object.ReferenceEquals(null, reader))
                            {
                                throw new ArgumentNullException(nameof(reader));
                            }

                            reader.CheckElementBegin("address");
                            reader.Forward();

                            while (true)
                            {
                                if (reader.IsElementEnd("address"))
                                {
                                    reader.Forward();
                                    break;
                                }

                                if (reader.IsElementBegin("name"))
                                {
                                    Name = reader.ReadStringValue("name");
                                    continue;
                                }

                                throw new Exception("Unexpected statement");
                            }
                        }

                        public AddressClass SetName(string name)
                        {
                            return new AddressClass(name);
                        }

                        public override string ToString()
                        {
                            StringBuilder sb = new StringBuilder();
                            sb.Append("{");
                            sb.AppendFormat("name: {0}", Name);
                            sb.Append("}");

                            return sb.ToString();
                        }

                        public override int GetHashCode()
                        {
                            return Name.GetHashCode();
                        }

                        private bool EqualsUnprotected(AddressClass other)
                        {
                            return Name.Equals(other.Name);
                        }

                        public bool Equals(AddressClass other)
                        {
                            if (Object.ReferenceEquals(this, other))
                            {
                                return true;
                            }
                            else if (Object.ReferenceEquals(null, other))
                            {
                                return false;
                            }
                            return EqualsUnprotected(other);
                        }

                        public override bool Equals(object obj)
                        {
                            if (Object.ReferenceEquals(this, obj))
                            {
                                return true;
                            }
                            else if (Object.ReferenceEquals(null, obj))
                            {
                                return false;
                            }

                            var other = obj as AddressClass;
                            if (other == null)
                            {
                                return false;
                            }

                            return EqualsUnprotected(other);
                        }
                    }

                    /// <summary>
                    /// Interface address/destination prefix
                    /// </summary>
                    public readonly IReadOnlyDictionary<string, AddressClass> Address;

                    public InetClass(IReadOnlyDictionary<string, AddressClass> address)
                    {
                        Address = address;
                    }

                    public InetClass(XmlHelper.XmlReaderHelper reader)
                    {
                        Contract.Requires(null != reader);
                        if (Object.ReferenceEquals(null, reader))
                        {
                            throw new ArgumentNullException(nameof(reader));
                        }

                        reader.CheckElementBegin("inet");
                        var address = new Dictionary<string, AddressClass>();

                        reader.Forward();
                        while (true)
                        {
                            if (reader.IsElementEnd("inet"))
                            {
                                reader.Forward();
                                break;
                            }

                            if (reader.IsElementBegin("address"))
                            {
                                var item = new AddressClass(reader);
                                // Used as a key, need to check for validity.
                                // For strings, we check also for whitespace (not only null)
                                if (String.IsNullOrWhiteSpace(item.Name))
                                {
                                    throw new Exception("Expecting a non-null or empty key here");
                                }

                                address.Add(item.Name, item);
                                continue;
                            }

                            // Unexpected element
                            throw new Exception("Unexpected statement");
                        }

                        this.Address = new ReadOnlyDictionary<string, AddressClass>(address);
                    }

                    public override string ToString()
                    {
                        StringBuilder sb = new StringBuilder();
                        sb.Append("{");
                        sb.Append("address: [");
                        bool first = true;
                        foreach (var addressValue in Address.Values)
                        {
                            if (first)
                            {
                                sb.Append(addressValue.ToString());
                                first = false;
                            }
                            else
                            {
                                sb.AppendFormat(", {0}", addressValue.ToString());
                            }
                        }

                        sb.Append("]");
                        sb.Append("}");
                        return sb.ToString();
                    }

                    private bool EqualsUnprotected(InetClass other)
                    {
                        bool equalAddress = false;
                        if (Object.ReferenceEquals(this.Address, other.Address))
                        {
                            equalAddress = true;
                        }
                        else if (this.Address.Count != other.Address.Count)
                        {
                            // not equal
                        }
                        else
                        {
                            equalAddress = true;
                            foreach (var pair in this.Address)
                            {
                                if (other.Address.ContainsKey(pair.Key) == false)
                                {
                                    equalAddress = false;
                                    break;
                                }

                                if (pair.Value.Equals(other.Address[pair.Key]) == false)
                                {
                                    equalAddress = false;
                                    break;
                                }
                            }
                        }

                        return equalAddress;
                    }

                    public bool Equals(InetClass other)
                    {
                        if (Object.ReferenceEquals(this, other))
                        {
                            return true;
                        }
                        else if (Object.ReferenceEquals(null, other))
                        {
                            return false;
                        }
                        else
                        {
                            return EqualsUnprotected(other);
                        }
                    }

                    public override bool Equals(object obj)
                    {
                        if (Object.ReferenceEquals(this, obj))
                        {
                            return true;
                        }
                        else if (Object.ReferenceEquals(null, obj))
                        {
                            return false;
                        }
                        else
                        {
                            var other = obj as InetClass;
                            if (Object.ReferenceEquals(null, other))
                            {
                                return false;
                            }
                            else
                            {
                                return EqualsUnprotected(other);
                            }
                        }
                    }

                    public override int GetHashCode()
                    {
                        // TODO: algorithm to compute hash value
                        // TODO: Should we transform to lazy value?
                        int hash = 13;

                        foreach (var pair in Address)
                        {
                            hash ^= pair.Key.GetHashCode();
                            hash ^= pair.Value.GetHashCode();
                        }

                        return hash;
                    }
                }

                /// <summary>
                /// IPv4 parameters
                /// </summary>
                public readonly InetClass Inet;

                public bool IsInetEnabled => !(Inet is null);

                /// <summary>
                /// IPv6 protocol parameters
                /// </summary>
                public class Inet6Class : IEquatable<Inet6Class>
                {
                    /// <summary>
                    /// Interface address/destination prefix
                    /// </summary>
                    public class AddressClass : IEquatable<AddressClass>
                    {
                        /// <summary>
                        /// Interface address/destination prefix
                        /// </summary>
                        public readonly string Name;

                        public AddressClass(string name)
                        {
                            Contract.Requires(null != name);
                            if (name is null)
                            {
                                throw new ArgumentNullException(nameof(name));
                            }

                            this.Name = name;
                        }

                        public AddressClass(XmlHelper.XmlReaderHelper reader)
                        {
                            Contract.Requires(null != reader);
                            if (reader is null)
                            {
                                throw new ArgumentNullException(nameof(reader));
                            }

                            reader.CheckElementBegin("address");
                            reader.Forward();

                            while (true)
                            {
                                if (reader.IsElementEnd("address"))
                                {
                                    reader.Forward();
                                    break;
                                }

                                if (reader.IsElementBegin("name"))
                                {
                                    Name = reader.ReadStringValue("name");
                                    continue;
                                }

                                // Unexpected element
                                throw new Exception("Unexpected statement");
                            }
                        }

                        public AddressClass SetName(string name)
                        {
                            return new AddressClass(name);
                        }

                        public override string ToString()
                        {
                            StringBuilder sb = new StringBuilder();
                            sb.Append("{");
                            sb.AppendFormat("name: {0}", Name);
                            sb.Append("}");

                            return sb.ToString();
                        }

                        public override int GetHashCode()
                        {
                            return Name.GetHashCode();
                        }

                        private bool EqualsUnprotected(AddressClass other)
                        {
                            return Name.Equals(other.Name);
                        }

                        public bool Equals(AddressClass other)
                        {
                            if (Object.ReferenceEquals(this, other))
                            {
                                return true;
                            }
                            else if (Object.ReferenceEquals(null, other))
                            {
                                return false;
                            }
                            return EqualsUnprotected(other);
                        }

                        public override bool Equals(object obj)
                        {
                            if (Object.ReferenceEquals(this, obj))
                            {
                                return true;
                            }
                            else if (Object.ReferenceEquals(null, obj))
                            {
                                return false;
                            }

                            var other = obj as AddressClass;
                            if (other == null)
                            {
                                return false;
                            }

                            return EqualsUnprotected(other);
                        }
                    }

                    /// <summary>
                    /// Interface address/destination prefix
                    /// </summary>
                    public readonly IReadOnlyDictionary<string, AddressClass> Address;

                    public Inet6Class()
                    {
                        Address = new Dictionary<string, AddressClass>();
                    }

                    public Inet6Class(IReadOnlyDictionary<string, AddressClass> address)
                    {
                        Address = address;
                    }

                    public Inet6Class(XmlHelper.XmlReaderHelper reader)
                    {
                        Contract.Requires(null != reader);
                        if (reader is null)
                        {
                            throw new ArgumentNullException(nameof(reader));
                        }

                        reader.CheckElementBegin("inet6");
                        var address = new Dictionary<string, AddressClass>();

                        reader.Forward();
                        while (true)
                        {
                            if (reader.IsElementEnd("inet6"))
                            {
                                reader.Forward();
                                break;
                            }

                            if (reader.IsElementBegin("address"))
                            {
                                var item = new AddressClass(reader);
                                if (String.IsNullOrWhiteSpace(item.Name))
                                {
                                    throw new Exception("Expecting a non-null or empty key here");
                                }

                                address.Add(item.Name, item);
                                continue;
                            }

                            // Unexpected element
                            throw new Exception("Unexpected statement");
                        }

                        this.Address = new ReadOnlyDictionary<string, AddressClass>(address);
                    }

                    public override string ToString()
                    {
                        StringBuilder sb = new StringBuilder();
                        sb.Append("{");
                        sb.Append("address: [");
                        bool first = true;
                        foreach (var addressValue in Address.Values)
                        {
                            if (first)
                            {
                                sb.Append(addressValue.ToString());
                                first = false;
                            }
                            else
                            {
                                sb.AppendFormat(", {0}", addressValue.ToString());
                            }
                        }

                        sb.Append("]");
                        sb.Append("}");
                        return sb.ToString();
                    }

                    private bool EqualsUnprotected(Inet6Class other)
                    {
                        bool equalAddress = false;
                        if (Object.ReferenceEquals(this.Address, other.Address))
                        {
                            equalAddress = true;
                        }
                        else if (this.Address.Count != other.Address.Count)
                        {
                            // not equal
                        }
                        else
                        {
                            equalAddress = true;
                            foreach (var pair in this.Address)
                            {
                                if (other.Address.ContainsKey(pair.Key) == false)
                                {
                                    equalAddress = false;
                                    break;
                                }

                                if (pair.Value.Equals(other.Address[pair.Key]) == false)
                                {
                                    equalAddress = false;
                                    break;
                                }
                            }
                        }

                        return equalAddress;
                    }

                    public bool Equals(Inet6Class other)
                    {
                        if (Object.ReferenceEquals(this, other))
                        {
                            return true;
                        }
                        else if (Object.ReferenceEquals(null, other))
                        {
                            return false;
                        }
                        else
                        {
                            return EqualsUnprotected(other);
                        }
                    }

                    public override bool Equals(object obj)
                    {
                        if (Object.ReferenceEquals(this, obj))
                        {
                            return true;
                        }
                        else if (Object.ReferenceEquals(null, obj))
                        {
                            return false;
                        }
                        else
                        {
                            var other = obj as Inet6Class;
                            if (Object.ReferenceEquals(null, other))
                            {
                                return false;
                            }
                            else
                            {
                                return EqualsUnprotected(other);
                            }
                        }
                    }

                    public override int GetHashCode()
                    {
                        // TODO: algorithm to compute hash value
                        // TODO: Should we transform to lazy value?
                        int hash = 13;

                        foreach (var pair in Address)
                        {
                            hash ^= pair.Key.GetHashCode();
                            hash ^= pair.Value.GetHashCode();
                        }

                        return hash;
                    }
                }

                /// <summary>
                /// IPv6 protocol parameters
                /// </summary>
                public readonly Inet6Class Inet6;

                public bool IsInet6Enabled => !(Inet6 is null);

                public UnitClass(
                    string name,
                    string description,
                    VlanChoiceClass vlanChoice,
                    InetClass inet,
                    Inet6Class inet6)
                {
                    this.Name = name;
                    this.Description = description;
                    this.VlanChoice = vlanChoice;
                    this.Inet = inet;
                    this.Inet6 = inet6;
                }

                public UnitClass(XmlHelper.XmlReaderHelper reader)
                {
                    if (reader is null)
                    {
                        throw new ArgumentNullException(nameof(reader));
                    }
                    Contract.EndContractBlock();

                    reader.CheckElementBegin("unit");
                    reader.Forward();

                    while (true)
                    {
                        if (reader.IsElementEnd("unit"))
                        {
                            reader.Forward();
                            break;
                        }

                        if (reader.IsElementBegin("name"))
                        {
                            Name = reader.ReadStringValue("name");
                            continue;
                        }

                        if (reader.IsElementBegin("description"))
                        {
                            Description = reader.ReadStringValue("description");
                            continue;
                        }

                        if (reader.IsElementBegin("vlan_choice"))
                        {
                            throw new NotImplementedException();
                        }

                        if (reader.IsElementBegin("family"))
                        {
                            reader.Forward();

                            while (true)
                            {
                                if (reader.IsElementEnd("family"))
                                {
                                    reader.Forward();
                                    break;
                                }

                                if (reader.IsElementBegin("inet"))
                                {
                                    Inet = new InetClass(reader);
                                    continue;
                                }

                                if (reader.IsElementBegin("inet6"))
                                {
                                    Inet6 = new Inet6Class(reader);
                                    continue;
                                }

                                throw new Exception("Unexpected statement");
                            }

                            continue;
                        }

                        // Unexpected element
                        throw new Exception("Unexpected statement");
                    }
                }

                public override string ToString()
                {
                    StringBuilder sb = new StringBuilder();

                    sb.Append("{");
                    sb.AppendFormat("name: {0}, ", Name);
                    sb.AppendFormat("description: {0}, ", Description);
                    sb.AppendFormat("vlan_choice: {0}, ", VlanChoice);
                    sb.AppendFormat("Inet: {0}, ", Inet);
                    sb.AppendFormat("Inet6: {0}", Inet6);
                    sb.Append("}");

                    return sb.ToString();
                }

                public bool Equals(UnitClass other)
                {
                    if (ReferenceEquals(null, other)) return false;
                    if (ReferenceEquals(this, other)) return true;

                    return  string.Equals(Name, other.Name) &&
                            string.Equals(Description, other.Description) &&
                            Equals(VlanChoice, other.VlanChoice) &&
                            Equals(Inet, other.Inet) &&
                            Equals(Inet6, other.Inet6)
                        ;
                }

                public override bool Equals(object obj)
                {
                    if (ReferenceEquals(null, obj)) return false;
                    if (ReferenceEquals(this, obj)) return true;
                    if (obj.GetType() != this.GetType()) return false;
                    return Equals((UnitClass) obj);
                }

                public override int GetHashCode()
                {
                    unchecked
                    {
                        var hashCode = (Name != null ? Name.GetHashCode() : 0);
                        hashCode = (hashCode * 397) ^ (Description != null ? Description.GetHashCode() : 0);
                        hashCode = (hashCode * 397) ^ (VlanChoice != null ? VlanChoice.GetHashCode() : 0);
                        hashCode = (hashCode * 397) ^ (Inet != null ? Inet.GetHashCode() : 0);
                        hashCode = (hashCode * 397) ^ (Inet6 != null ? Inet6.GetHashCode() : 0);
                        return hashCode;
                    }
                }
            }

            /// <summary>
            /// Logical interface
            /// </summary>
            public readonly IReadOnlyDictionary<string, UnitClass> Unit;

            public InterfaceClass(string name, string description, UInt32 mtu, IReadOnlyDictionary<string, UnitClass> unit)
            {
                if (string.IsNullOrWhiteSpace(name))
                {
                    throw new ArgumentNullException(nameof(name));
                }
                if (!_mtu_Check(mtu))
                {
                    throw new ArgumentOutOfRangeException(nameof(mtu), "Value should be in range: " + _mtu_Range);
                }

                Name = name;
                Description = description;
                Mtu = mtu;
                Unit = unit;
            }

            public InterfaceClass(XmlHelper.XmlReaderHelper reader)
            {
                if (reader is null)
                {
                    throw new ArgumentNullException(nameof(reader));
                }

                Contract.EndContractBlock();

                // TODO: Default value for MTU if not specified?

                reader.CheckElementBegin("interface");
                reader.Forward();
                var unit = new Dictionary<string, UnitClass>();

                while (true)
                {
                    if (reader.IsElementEnd("interface"))
                    {
                        reader.Forward();
                        break;
                    }

                    if (reader.IsElementBegin("name"))
                    {
                        Name = reader.ReadStringValue("name");
                        continue;
                    }

                    if (reader.IsElementBegin("description"))
                    {
                        Description = reader.ReadStringValue("description");
                        continue;
                    }

                    if (reader.IsElementBegin("mtu"))
                    {
                        var value = reader.ReadStringValue("mtu");
                        Mtu = UInt32.Parse(value);
                        if (!_mtu_Check(Mtu))
                        {
                            throw new ArgumentOutOfRangeException(nameof(Mtu), "Value should be in range: " + _mtu_Range);
                        }

                        continue;
                    }

                    if (reader.IsElementBegin("unit"))
                    {
                        var element = new UnitClass(reader);
                        unit.Add(element.Name, element);
                        continue;
                    }

                    throw new Exception("Unexpected statement");
                }

                Unit = unit;
            }

            public override string ToString()
            {
                var sb = new StringBuilder();

                sb.Append("{ ");

                sb.AppendFormat("name: {0}, ", Name);
                sb.AppendFormat("description: {0}, ", Description);
                sb.AppendFormat("mtu: {0}, ", Mtu);

                sb.Append("unit: [ ");
                bool isFirst = true;

                foreach (var value in Unit.Values)
                {
                    if (isFirst)
                    {
                        sb.AppendFormat("{0}", value);
                        isFirst = false;
                    }
                    else
                    {
                        sb.AppendFormat(", {0}", value);
                    }
                }
                sb.Append(" ]");

                sb.Append(" }");

                return sb.ToString();
            }
        }

        /// <summary>
        /// Physical interface
        /// </summary>
        public readonly IReadOnlyDictionary<string, InterfaceClass> Interface;

        public Configuration(string version, IReadOnlyDictionary<string, InterfaceClass> _interface)
        {
            Version = version;
            Interface = _interface;
        }

        public Configuration(XmlHelper.XmlReaderHelper reader)
        {
            if (reader is null)
            {
                throw new ArgumentNullException(nameof(reader));
            }

            Contract.EndContractBlock();

            reader.CheckElementBegin("configuration");
            reader.Forward();
            var _interface = new Dictionary<string, InterfaceClass>();

            while (true)
            {
                if (reader.IsElementEnd("configuration"))
                {
                    reader.Forward();
                    break;
                }

                if (reader.IsElementBegin("version"))
                {
                    Version = reader.ReadStringValue("version");
                    continue;
                }

                if (reader.IsElementBegin("system"))
                {
                    reader.Forward();

                    while (true)
                    {
                        if (reader.IsElementEnd("system"))
                        {
                            reader.Forward();
                            break;
                        }

                        if (reader.IsElementBegin("interfaces"))
                        {
                            reader.Forward();

                            while (true)
                            {
                                if (reader.IsElementEnd("interfaces"))
                                {
                                    reader.Forward();
                                    break;
                                }

                                if (reader.IsElementBegin("interface"))
                                {
                                    var value = new InterfaceClass(reader);
                                    _interface.Add(value.Name, value);
                                    continue;
                                }

                                throw new Exception("Unexpected statement");
                            }

                            continue;
                        }

                        throw new Exception("Unexpected statement");
                    }

                    continue;
                }

                throw new Exception("Unexpected statement");
            }

            Interface = _interface;
        }

        public override string ToString()
        {
            var sb = new StringBuilder();
            sb.Append("{ ");

            sb.AppendFormat("version: {0}, ", Version);

            sb.Append("interface: [");

            bool isFirst = true;
            foreach (var value in Interface.Values)
            {
                if (isFirst)
                {
                    sb.Append(value);
                    isFirst = false;
                }
                else
                {
                    sb.AppendFormat(", {0},", value);
                }
            }

            sb.Append("] ");
            sb.Append(" }");
            return sb.ToString();
        }
    }
}
